#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

using pip

# Installing virtualfish for python virtual env management
info 'Installing virtualfish..'  # https://github.com/adambrenecki/virtualfish
run pip install --user virtualfish


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

info 'Downloading fish installer.'
run get_fish_installer


# =================================================================
#  CHECKING VERSION

info "Uninstalling existing OMF installation.."
run fish ${OMF_INSTALLER} --uninstall --yes

info "Installing OMF.. (this could take a while..)"
run fish ${OMF_INSTALLER} --noninteractive

popd&>/dev/null
rm -rf ${OMF_INSTALLER_DIR}

