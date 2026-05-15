use agda_mcp::agda::{command::Command, process::AgdaProcess};

#[tokio::test]
async fn show_version_returns_version_display_info() {
    let mut agda = AgdaProcess::spawn()
        .await
        .expect("failed to spawn agda --interaction-json");

    let responses = agda
        .send(&Command::show_version("."))
        .await
        .expect("failed to send Cmd_show_version to Agda");

    assert!(
        responses.iter().any(|response| {
            response.get("kind").and_then(|kind| kind.as_str()) == Some("DisplayInfo")
                && response
                    .get("info")
                    .and_then(|info| info.get("kind"))
                    .and_then(|kind| kind.as_str())
                    == Some("Version")
        }),
        "expected a DisplayInfo/Version response, got: {responses:#?}"
    );
}
