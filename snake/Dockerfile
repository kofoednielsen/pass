from rust

workdir /app
copy Cargo.toml Cargo.lock .
copy src/ ./src/
run cargo build --release
cmd ./target/release/pass 0.0.0.0:8080
