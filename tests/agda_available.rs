use std::process::Command;

#[test]
fn agda_binary_is_available() {
    let agda_bin = std::env::var("AGDA_BIN").unwrap_or_else(|_| "agda".to_owned());

    let output = Command::new(&agda_bin)
        .arg("--version")
        .output()
        .unwrap_or_else(|error| {
            panic!(
                "failed to run Agda binary `{agda_bin}`: {error}\n\
                 Set AGDA_BIN or run tests inside `nix develop`, where Agda is provided."
            )
        });

    assert!(
        output.status.success(),
        "Agda binary `{agda_bin}` exited with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
