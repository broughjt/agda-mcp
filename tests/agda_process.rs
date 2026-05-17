use agda_mcp::agda::{
    command::Command,
    process::AgdaProcess,
    response::{Info, Response},
};

#[tokio::test]
async fn show_version_returns_version_display_info() {
    let mut agda = AgdaProcess::spawn()
        .await
        .expect("failed to spawn agda --interaction-json");

    let responses = agda
        .send(&Command::show_version("."))
        .await
        .expect("failed to send Cmd_show_version to Agda");

    let version = responses.iter().find_map(|response| match response {
        Response::DisplayInfo {
            info: Info::Version { version },
        } => Some(version.clone()),
        _ => None,
    });

    assert!(
        version.is_some(),
        "expected a DisplayInfo/Version response, got parsed: {responses:#?}"
    );
}
