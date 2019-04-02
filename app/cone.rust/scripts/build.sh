#! /bin/sh

cargo web build --release
echo "installing ..."
cp -f target/wasm32-unknown-unknown/release/cone* ../resources
