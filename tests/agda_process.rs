use agda_mcp::agda::{
    command::Command,
    process::AgdaProcess,
    response::{Info, KnownInfo, KnownResponse, Response},
};

#[tokio::test]
async fn show_version_returns_version_display_info() {
    let mut agda = AgdaProcess::spawn()
        .await
        .expect("failed to spawn agda --interaction-json");

    let raw = agda
        .send(&Command::show_version("."))
        .await
        .expect("failed to send Cmd_show_version to Agda");
    let parsed = Response::parse_all(&raw);

    let version = parsed.iter().find_map(|response| match response {
        Response::Known(known) => match known.as_ref() {
            KnownResponse::DisplayInfo {
                info: Info::Known(info),
            } => match info.as_ref() {
                KnownInfo::Version { version } => Some(version.clone()),
                _ => None,
            },
            _ => None,
        },
        Response::Unknown(_) => None,
    });

    assert!(
        version.is_some(),
        "expected a DisplayInfo/Version response, got raw: {raw:#?}, parsed: {parsed:#?}"
    );
}
