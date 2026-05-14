use rmcp::{
    ServerHandler, ServiceExt, handler::server::wrapper::Parameters, tool, tool_handler,
    tool_router, transport::stdio,
};
use schemars::JsonSchema;
use serde::Deserialize;

#[derive(Clone)]
struct AgdaMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
struct HelloParams {
    /// Optional name to greet.
    name: Option<String>,
}

#[tool_router]
impl AgdaMcpServer {
    #[tool(description = "Return a fake hello greeting")]
    fn hello(&self, Parameters(HelloParams { name }): Parameters<HelloParams>) -> String {
        let name = name.unwrap_or_else(|| "world".to_owned());
        format!("Hello, {name}!")
    }
}

#[tool_handler(
    name = "agda-mcp",
    version = "0.1.0",
    instructions = "Stub MCP server for Agda with a fake hello tool."
)]
impl ServerHandler for AgdaMcpServer {}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let service = AgdaMcpServer.serve(stdio()).await?;
    service.waiting().await?;
    Ok(())
}
