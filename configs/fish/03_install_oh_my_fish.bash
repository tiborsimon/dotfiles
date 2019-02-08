#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

using pip
using jq

# Installing virtualfish for python virtual env management
info 'Installing virtualfish..'  # https://github.com/adambrenecki/virtualfish
execute pip install --user virtualfish


# OMF will be in the following directories
#  ~/.cache/omf
#  ~/.config/omf
#  ~/.local/share/omf

# Cleaning up caches
# rm -rf ${HOME}/.cache/omf
# rm -rf ${HOME}/.local/share/omf

OMF_INSTALLER_DIR='omf_tmp'
OMF_INSTALLER='install'
OMF_INSTALLER_CHECKSUM='install_sum'

# creating temporary installer direcotry
rm -rf ${OMF_INSTALLER_DIR}
mkdir ${OMF_INSTALLER_DIR}
pushd ${OMF_INSTALLER_DIR}&>/dev/null

function finish {
  popd &> /dev/null
  rm -rf ${OMF_INSTALLER_DIR}
}
trap finish EXIT


# =================================================================
#  GETTING FISH INSTALLER

function get_fish_installer {
  # Getting the installer and the checksum file
  curl -sL https://get.oh-my.fish > ${OMF_INSTALLER}
  curl -sL https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install.sha256 > ${OMF_INSTALLER_CHECKSUM}

  # Comparing the installer to the checksum file
  if ! sha256sum ${OMF_INSTALLER} | sha256sum -c ${OMF_INSTALLER_CHECKSUM}; then
    echo "OMF installer is invalid!!!! Aborting.."
    return 1
  fi
}

info 'Downloading fish installer..'
execute get_fish_installer


# =================================================================
#  CHECKING VERSION

function install_fish {
  latest_version=$(curl -s https://api.github.com/repos/oh-my-fish/oh-my-fish/releases/latest | jq -r '.name')
  if current_version=$(fish -c 'omf version'); then
    latest_version=$(echo $latest_version | cut -d' ' -f2)
    current_version=$(echo $current_version | cut -d' ' -f5)
    if [ "$latest_version" = "$current_version" ]; then
      return 0
    else
      info "Uninstalling existing OMF installation.."
      execute fish ${OMF_INSTALLER} --uninstall --yes
    fi
  fi

  info "Installing OMF.. (this could take a while..)"
  execute fish ${OMF_INSTALLER} --noninteractive
}

info 'Installing fish..'
execute install_fish
