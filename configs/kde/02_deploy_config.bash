#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

#=============================================================================
# KDE
#=============================================================================

link_package \
  "./config/kde/kdeglobals" "${HOME}/.config/kdeglobals" \
  "./config/kde/kglobalshortcutsrc" "${HOME}/.config/kglobalshortcutsrc" \
  "./config/kde/khotkeysrc" "${HOME}/.config/khotkeysrc" \
  "./config/kde/kwinrc" "${HOME}/.config/kwinrc" \
  "./config/kde/Trolltech.conf" "${HOME}/.config/Trolltech.conf" \
  "./config/kde/.kde4" "${HOME}/.kde4" \
  "./config/kde/kcminputrc" "${HOME}/.config/kcminputrc" \
  "./config/kde/powermanagementprofilesrc" "${HOME}/.config/powermanagementprofilesrc" \
  "./config/kde/baloofilerc" "${HOME}/.config/baloofilerc" \
  "./config/kde/touchpadxlibinputrc" "${HOME}/.config/touchpadxlibinputrc" \
  "./config/kde/breezerc" "${HOME}/.config/breezerc" \
  "./config/kde/plasma-org.kde.plasma.desktop-appletsrc" "${HOME}/.config/plasma-org.kde.plasma.desktop-appletsrc" \
  "./config/kde/plasmashellrc" "${HOME}/.config/plasmashellrc" \

#=============================================================================
# Konsole
#=============================================================================

mkdir -p ~/.local/share/konsole/shortcuts
link_package \
  "./config/konsole/konsolerc" "${HOME}/.config/konsolerc" \
  "./config/konsole/tibor.profile" "${HOME}/.local/share/konsole/tibor.profile" \
  "./config/konsole/tibor.shortcuts" "${HOME}/.local/share/konsole/shortcuts/tibor" \

#=============================================================================
# GTK
#=============================================================================

link_package \
  "./config/gtk/gtk-3.0" "${HOME}/.config/gtk-3.0" \
  "./config/gtk/gtk-4.0" "${HOME}/.config/gtk-4.0" \
  "./config/gtk/gtkrc"  "${HOME}/.config/gtkrc" \
  "./config/gtk/gtkrc-2.0" "${HOME}/.config/gtkrc-2.0" \

#=============================================================================
# Yakuake
#=============================================================================

mkdir -p ~/.local/share/yakuake/shortcuts
link_package \
  "./config/yakuake/yakuakerc" "${HOME}/.config/yakuakerc" \
  "./config/yakuake/tibor.shortcuts" "${HOME}/.local/share/yakuake/shortcuts/tibor" \

