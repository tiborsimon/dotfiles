#!/usr/bin/env bash
#######################################
# Generates make targets for the configurations.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

TARGET_MAKEFILE='Makefile.targets'

set -e

cd $(dirname $(readlink -f $0))

configs=$(cd ../configs; ls | grep -v '.bash' | grep -v Makefile | sort)


rm $TARGET_MAKEFILE
for config in $configs; do
  echo "Generating target for config $config.."
  echo ".PHONY: install-$config" >> $TARGET_MAKEFILE
  echo "install-$config:" >> $TARGET_MAKEFILE
  echo "	@./utils/install-configs.bash $config" >> $TARGET_MAKEFILE
  echo "" >> $TARGET_MAKEFILE
done

