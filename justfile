test:
    cargo test --workspace -- --nocapture
    
watch-test:
    cargo watch --ignore 'snapshots/*' -- cargo test --workspace  -- --nocapture --