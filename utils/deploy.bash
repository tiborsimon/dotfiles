#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

configs=$(find ../configs/ -type f -name deploy.bash | sort)

for config in $configs; do
  dir=$(dirname $config)
  pushd $dir &> /dev/null
  ./deploy.bash
  popd &> /dev/null
done

