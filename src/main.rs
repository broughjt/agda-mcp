use std::sync::Arc;

use rmcp::{
    ErrorData, ServerHandler, ServiceExt,
    handler::server::wrapper::Parameters,
    model::{CallToolResult, Content},
    tool, tool_handler, tool_router,
    transport::stdio,
};
use tokio::sync::Mutex;
use tokio_util::sync::CancellationToken;

use agda_mcp::server::ServerState;
use agda_mcp::tools::{LoadRequest, LoadResponse};

#[derive(Clone)]
struct AgdaMcpServer {
    state: Arc<Mutex<ServerState>>,
}

impl AgdaMcpServer {
    fn new(state: ServerState) -> Self {
        Self {
            state: Arc::new(Mutex::new(state)),
        }
    }
}

#[tool_router]
impl AgdaMcpServer {
    #[tool(
        description = "Type-check an Agda file and return its goals, warnings, and errors.",
        output_schema = rmcp::handler::server::common::schema_for_output::<LoadResponse>()
            .expect("LoadOutput should produce an object JSON schema")
    )]
    async fn load(
        &self,
        Parameters(params): Parameters<LoadRequest>,
    ) -> Result<CallToolResult, ErrorData> {
        let mut state = self.state.lock().await;
        let output = state.load(&params).await?;

        // Pack the load result into an MCP tool result with both a
        // human-readable text summary (`content`) and the typed structured
        // payload (`structured_content`). LLM clients read the text;
        // programmatic clients consume the structured object against the
        // published output schema.
        let mut result = CallToolResult::default();
        result.content = vec![Content::text(output.to_string())];
        result.structured_content =
            Some(serde_json::to_value(&output).expect("LoadOutput serializes cleanly"));
        // Agda type/checking errors are the expected domain-level result of a
        // successful `load` tool call, not an MCP transport/tool failure. If we
        // mark the MCP result itself as an error, clients such as pi prepend
        // their own `Error:` label to the text summary, producing confusing
        // output like `Error: Error:` and sometimes adding tool-schema help.
        result.is_error = Some(false);

        Ok(result)
    }
}

#[tool_handler(
    name = "agda-mcp",
    version = "0.1.0",
    instructions = "MCP server for Agda. Call `load` with the path to an Agda file to type-check it and inspect interaction goals."
)]
impl ServerHandler for AgdaMcpServer {}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let shutdown = CancellationToken::new();
    let state = ServerState::spawn(shutdown.clone()).await?;
    let service = AgdaMcpServer::new(state).serve(stdio()).await?;

    // When `shutdown` fires (e.g. a tool observed a protocol-fatal error),
    // cancel the rmcp service's internal token. That makes the service loop
    // drain its queue, flush the in-flight tool response back to the client,
    // and return, so that `service.waiting()` below resolves cleanly.
    let service_token = service.cancellation_token();
    let shutdown_listener = shutdown.clone();
    tokio::spawn(async move {
        shutdown_listener.cancelled().await;
        tracing::error!("shutting down MCP service: Agda interaction protocol error");
        service_token.cancel();
    });

    service.waiting().await?;
    Ok(())
}
