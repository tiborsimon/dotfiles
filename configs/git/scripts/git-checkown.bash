#!/usr/bin/env bash
set -e

user=$(git config user.name)

branch=$(git for-each-ref --format='%(authorname)=%(refname)' --sort=-authordate  |
  grep -i "$user" |
  cut -d'=' -f2 |
  sed 's%refs/heads/%%' |
  sed 's%refs/remotes/origin/%%' |
  uniq |
  fzy)

if [ -n "$branch" ]
then
  git checkout $branch
fi
