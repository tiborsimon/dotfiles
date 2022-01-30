#!/bin/sh

# Enable VSCode to run on wayland.
# Based on this blog: https://www.fosskers.ca/en/blog/wayland
code --enable-features=UseOzonePlatform --ozone-platform=wayland
