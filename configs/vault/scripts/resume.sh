#!/bin/sh

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# RESUME THE VAULT
#==============================================================================


echo " STEP 1/1 | Resuming luks device.."
sudo cryptsetup luksResume "$VAULT__LUKS_NAME"
echo " STEP 1/1 | Done"
