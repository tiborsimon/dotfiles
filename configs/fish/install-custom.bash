#!/usr/bin/env bash
# Cd into the scripts location
cd $(dirname $(readlink -f $0))

# Installing virtualfish for python virtual env management
# https://github.com/adambrenecki/virtualfish
echo 'Installing virtualfish..'
pip install --user virtualfish

# Installing Oh My Fish
echo 'Installing Oh My Fish..'

# OMF will be in the following directories
#  ~/.cache/omf
#  ~/.config/omf
#  ~/.local/share/omf

OMF_INSTALLER_DIR='omf_tmp'
OMF_INSTALLER='install'
OMF_INSTALLER_CHECKSUM='install_sum'

rm -rf ${OMF_INSTALLER_DIR}
mkdir ${OMF_INSTALLER_DIR}
pushd ${OMF_INSTALLER_DIR}&>/dev/null

# Getting the installer and the checksum file
curl -sL https://get.oh-my.fish > ${OMF_INSTALLER}
curl -sL https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install.sha256 > ${OMF_INSTALLER_CHECKSUM}

# Comparing the installer to the checksum file
if sha256sum ${OMF_INSTALLER} | sha256sum -c ${OMF_INSTALLER_CHECKSUM}; then
  echo "OMF installer verified! Installing.."
  fish ${OMF_INSTALLER} --uninstall --yes
  fish ${OMF_INSTALLER} --noninteractive
else
  echo "OMF installer is invalid!!!! Aborting.."
fi

popd&>/dev/null
rm -rf ${OMF_INSTALLER_DIR}

