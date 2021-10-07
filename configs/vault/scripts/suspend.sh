#!/bin/sh

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# SUSPENDING THE VAULT
#==============================================================================

echo " STEP 1/3 | Synching fs to disk.."
sync
echo " STEP 1/3 | Done"

echo " STEP 2/3 | Suspending luks volume.."
sudo cryptsetup luksSuspend "$VAULT__LUKS_NAME"
echo " STEP 2/3 | Done"

echo " STEP 3/3 | Flushing disk cache.."
# https://www.kernel.org/doc/Documentation/sysctl/vm.txt
echo 3 | sudo tee /proc/sys/vm/drop_caches >/dev/null
echo " STEP 3/3 | Done"
