use std::fs;

use agda_mcp::server::ServerState;
use agda_mcp::tools::LoadRequest;
use tempfile::tempdir;
use tokio_util::sync::CancellationToken;

const SPIKE_SOURCE: &str = "module Spike where
open import Agda.Builtin.Nat

x : Nat
x = {! !}
";

#[tokio::test]
async fn load_spike_file_returns_single_nat_goal() {
    let dir = tempdir().expect("failed to create temp directory");
    let spike_path = dir.path().join("Spike.agda");
    fs::write(&spike_path, SPIKE_SOURCE).expect("failed to write Spike.agda");

    let shutdown = CancellationToken::new();
    let mut state = ServerState::spawn(shutdown.clone())
        .await
        .expect("failed to spawn Agda interaction process");
    let output = state
        .load(&LoadRequest {
            path: spike_path.to_string_lossy().into_owned(),
        })
        .await
        .expect("load should succeed against a well-formed file");

    assert!(
        !output.checked,
        "Agda reports checked=false while interaction goals remain open"
    );
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);

    assert_eq!(
        output.goals.len(),
        1,
        "expected exactly one visible goal, got {:#?}",
        output.goals
    );
    let goal = &output.goals[0];
    assert_eq!(goal.id, 0, "expected goal id 0, got {}", goal.id);
    assert!(
        goal._type.contains("Nat"),
        "expected goal type to contain `Nat`, got {:?}",
        goal._type
    );
    assert!(
        !goal.range.is_empty(),
        "expected non-empty range on goal: {goal:?}"
    );
    let interval = goal.range[0];
    assert!(
        interval.start.pos < interval.end.pos,
        "expected goal range to span at least one character: {interval:?}"
    );

    // The session should remember the load.
    let expected_path = fs::canonicalize(&spike_path)
        .expect("canonicalise spike path")
        .to_string_lossy()
        .into_owned();
    let loaded = state.loaded().expect("state should cache the loaded file");
    assert_eq!(loaded.current_file, expected_path);
    assert_eq!(loaded.goals.len(), 1);

    // A successful round-trip must not have signalled shutdown.
    assert!(
        !shutdown.is_cancelled(),
        "shutdown was triggered after a clean load"
    );
}
