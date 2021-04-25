#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  "./config/gtk-3.0" "${HOME}/.config/gtk-3.0" \
  "./config/gtk-4.0" "${HOME}/.config/gtk-4.0" \
  "./config/gtkrc"  "${HOME}/.config/gtkrc" \
  "./config/gtkrc-2.0" "${HOME}/.config/gtkrc-2.0" \
  "./config/kdeglobals" "${HOME}/.config/kdeglobals" \
  "./config/kglobalshortcutsrc" "${HOME}/.config/kglobalshortcutsrc" \
  "./config/khotkeysrc" "${HOME}/.config/khotkeysrc" \
  "./config/konsolerc" "${HOME}/.config/konsolerc" \
  "./config/kwinrc" "${HOME}/.config/kwinrc" \
  "./config/Trolltech.conf" "${HOME}/.config/Trolltech.conf" \
  "./config/.kde4" "${HOME}/.kde4" \
  "./config/kcminputrc" "${HOME}/.config/kcminputrc" \
  "./config/powermanagementprofilesrc" "${HOME}/.config/powermanagementprofilesrc" \
  "./config/baloofilerc" "${HOME}/.config/baloofilerc" \
  "./config/touchpadxlibinputrc" "${HOME}/.config/touchpadxlibinputrc" \
  "./config/breezerc" "${HOME}/.config/breezerc" \
  "./config/plasma-org.kde.plasma.desktop-appletsrc" "${HOME}/.config/plasma-org.kde.plasma.desktop-appletsrc" \
  "./config/plasmashellrc" "${HOME}/.config/plasmashellrc" \

