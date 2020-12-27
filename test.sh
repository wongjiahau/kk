#!/bin/sh
cargo build
cargo test run_all_tests -- --nocapture
