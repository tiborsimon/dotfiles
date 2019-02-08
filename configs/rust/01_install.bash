#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

info "Installing the rust toolset.."
function install_rust {
  curl -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
}
execute install_rust

info "Downloading nightly rust.."
execute rustup install nightly
