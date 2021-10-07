#!/bin/sh

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# CLOSING UP THE VAULT
#==============================================================================

echo " STEP 1/2 | Unmounting luks device.."
sudo umount -f "$VAULT__MOUNT_POINT" | true
echo " STEP 1/2 | Done"

echo " STEP 2/2 | Closing luks device.."
sudo cryptsetup luksClose "$VAULT__LUKS_NAME"
echo " STEP 2/2 | Done"
