use std::fs;

use agda_mcp::server::ServerState;
use agda_mcp::tools::{GiveRequest, GiveResponse, LoadRequest};
use tempfile::tempdir;
use tokio_util::sync::CancellationToken;

const ASCII_SOURCE: &str = "module Spike where
open import Agda.Builtin.Nat

x : Nat
x = {! !}
";

const ASCII_EXPECTED: &str = "module Spike where
open import Agda.Builtin.Nat

x : Nat
x = zero
";

/// Same shape as the ASCII source but with a multi-byte UTF-8 character
/// (Greek α, 2 bytes) on a comment line before the hole. The char-based
/// position of `{! !}` is identical to the ASCII case, but the byte
/// offset differs — so any byte/char confusion in the edit pipeline
/// produces a mangled file.
const UNICODE_SOURCE: &str = "module Spike where
open import Agda.Builtin.Nat

-- α
x : Nat
x = {! !}
";

const UNICODE_EXPECTED: &str = "module Spike where
open import Agda.Builtin.Nat

-- α
x : Nat
x = zero
";

async fn give_zero_into_single_hole(label: &str, source: &str, expected: &str) {
    let dir = tempdir().expect("failed to create temp directory");
    let spike_path = dir.path().join("Spike.agda");
    fs::write(&spike_path, source).expect("failed to write Spike.agda");
    let path = spike_path.to_string_lossy().into_owned();

    let shutdown = CancellationToken::new();
    let mut state = ServerState::spawn(shutdown.clone())
        .await
        .expect("failed to spawn Agda interaction process");

    // `give` requires that the file has been loaded so Agda knows about
    // interaction point `?0`.
    state
        .load(&LoadRequest { path: path.clone() })
        .await
        .unwrap_or_else(|error| panic!("[{label}] initial load failed: {error}"));

    let output = state
        .give(&GiveRequest {
            path: path.clone(),
            goal_id: 0,
            expression: "zero".to_owned(),
        })
        .await
        .unwrap_or_else(|error| panic!("[{label}] give returned a protocol-fatal error: {error}"))
        .unwrap_or_else(|error| panic!("[{label}] give edit failed: {error}"));

    match &output.give {
        GiveResponse::Accepted {
            interaction_point, ..
        } => assert_eq!(
            interaction_point.id, 0,
            "[{label}] expected give to act on ?0"
        ),
        GiveResponse::Rejected { error } => panic!("[{label}] give was rejected: {error}"),
    }

    let after = fs::read_to_string(&spike_path).expect("read edited file");
    assert_eq!(
        after, expected,
        "[{label}] file content mismatch after give"
    );

    assert!(
        output.reload.checked,
        "[{label}] reload should report checked=true; got reload: {:#?}",
        output.reload
    );
    assert!(
        output.reload.goals.is_empty(),
        "[{label}] expected no goals after give: {:#?}",
        output.reload.goals
    );
    assert!(
        output.reload.errors.is_empty(),
        "[{label}] expected no errors after give: {:#?}",
        output.reload.errors
    );

    assert!(
        !shutdown.is_cancelled(),
        "[{label}] shutdown was triggered after a clean give"
    );
}

#[tokio::test]
async fn give_zero_fills_ascii_hole_and_reloads_clean() {
    give_zero_into_single_hole("ascii", ASCII_SOURCE, ASCII_EXPECTED).await;
}

#[tokio::test]
async fn give_zero_fills_unicode_hole_and_reloads_clean() {
    give_zero_into_single_hole("unicode", UNICODE_SOURCE, UNICODE_EXPECTED).await;
}
