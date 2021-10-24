#!/bin/sh

# Enable keybase to run on wayland.
# Based on this blog: https://www.fosskers.ca/en/blog/wayland
keybase-gui --enable-features=UseOzonePlatform --ozone-platform=wayland
