#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# https://github.com/syl20bnr/spacemacs/issues/10434#issuecomment-444267037
function patch_org_projectile {
  cd ~/.emacs.d
  if ! git branch | grep -q fix-org-projectile; then
    git checkout -b fix-org-projectile
    git cherry-pick 6063466
    rm CHANGELOG.develop
    git add -A .
    git -c core.editor=true cherry-pick --continue
  fi
}

run patch_org_projectile
