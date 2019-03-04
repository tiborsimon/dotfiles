#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

using pip
using pipsi

install_packages tk

# ===========================================================================
#  PIP INSTALLS

info "Installing virtualenvwrapper.."
execute pip install --user virtualenvwrapper

info "Installing jedi.."
execute pip install --user jedi

info "Installing ipython.."
execute pip install --user ipython


# ===========================================================================
#  PIPSI INSTALLS

# this patch function is needed until the output handling issue is resolved..
# https://github.com/mitsuhiko/pipsi/issues/95
function pipsi_patch {
  output=$( { pipsi $@ 1>&2; } 2>&1 )
  if [ $? != 0 ]; then
    if echo "$output" | grep -q already; then
      echo "$output"
      return 0
    else
      echo "$output"
      return 1
    fi
  fi
}

info "Installing youtube-dl.."
execute pipsi_patch install youtube-dl

info "Installing cheat.."
execute pipsi_patch install cheat

info "Installing cookiecutter.."
execute pipsi_patch install cookiecutter

info "Installing flake8.."
execute pipsi_patch install flake8

info "Installing httpie.."
execute pipsi_patch install httpie
