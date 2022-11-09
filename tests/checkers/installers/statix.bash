# INSTALL RUSTUP
curl --proto '=https' --tlsv1.3 https://sh.rustup.rs -sSf | sh -s -- -y
. "$HOME/.cargo/env"

# BUILD STATIX
git clone "https://github.com/nerdypepper/statix"
cd statix
cargo build --release --features json

# INSTALL STATIX
ln -s "$(pwd)/target/release/statix" /usr/local/bin/statix
