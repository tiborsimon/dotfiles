#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

info "You should install the Krohnkite kwin plugin first manually!"
info "Settings -> Window Management -> KWin Scripts -> Get New Scripts.."
read -p " ?? | Press any key if you have installed Krohnkite. "

# Linking the Krohnkite config into the necessary place to make KDE display its
# config in the system settings gui.
mkdir -p ~/.local/share/kservices5/
if [ ! -L ~/.local/share/kservices5/krohnkite.desktop ]
then
  ln -s \
    ~/.local/share/kwin/scripts/krohnkite/metadata.desktop\
    ~/.local/share/kservices5/krohnkite.desktop
  success "Linking Krohnkite config into the kserrvices5 directory.."
fi
