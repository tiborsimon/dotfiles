#!/bin/sh

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# OPENING UP THE VAULT
#==============================================================================

echo " STEP 1/3 | Making sure mounting point exists.."
mkdir -p "$VAULT__MOUNT_POINT"
echo " STEP 1/3 | Done"

echo " STEP 2/3 | Opening luks device.."
sudo cryptsetup luksOpen "$VAULT__LUKS_DEVICE" "$VAULT__LUKS_NAME"
echo " STEP 2/3 | Done"

echo " STEP 3/3 | Mounting luks device.."
sudo mount /dev/mapper/vault "$VAULT__MOUNT_POINT"
echo " STEP 3/3 | Done"
