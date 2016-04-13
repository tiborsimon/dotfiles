#!/bin/bash

# git-branch-status - this script prints out pretty git branch sync status reports
#   * originally                              by http://github.com/jehiah
#   * "s'all good!" message                   by http://github.com/kd35a
#   * ANSI colors                             by http://github.com/knovoselic
#   * formatting, filters, usage, and remotes by http://github.com/bill-auger


read -r -d '' USAGE <<-'USAGE'
usage:

  git-branch-status
  git-branch-status [-a | --all]
  git-branch-status [-b | --branch] [branch-name]
  git-branch-status [-d | --dates]
  git-branch-status [-h | --help]
  git-branch-status [-r | --remotes]
  git-branch-status [-v | --verbose]

examples:

  # show only branches for which upstream HEAD differs from local
  $ git-branch-status
    | collab-branch  | (behind 1) | (ahead 2) | origin/collab-branch  |
    | feature-branch | (even)     | (ahead 2) | origin/feature-branch |
    | master         | (behind 1) | (even)    | origin/master         |

  # show all branches - even those with no upstream or no local and those up-to-date
  $ git-branch-status -a
  $ git-branch-status --all
    | master         | (even)     | (ahead 1) | origin/master             |
    | tracked-branch | (even)     | (even)    | origin/tracked-branch     |
    | (no local)     | n/a        | n/a       | origin/untracked-branch   |
    | local-branch   | n/a        | n/a       | (no upstream)             |
    | master         | (behind 1) | (ahead 1) | a-remote/master           |
    | (no local)     | n/a        | n/a       | a-remote/untracked-branch |

  # show the current branch
  $ git-branch-status -b
  $ git-branch-status --branch
    | current-branch | (even) | (ahead 2) | origin/current-branch |

  # show a specific branch
  $ git-branch-status          specific-branch
  $ git-branch-status -b       specific-branch
  $ git-branch-status --branch specific-branch
    | specific-branch | (even) | (ahead 2) | origin/specific-branch |

  # show the timestamp of each HEAD
  $ git-branch-status -d
  $ git-branch-status --dates
    | 1999-12-31 master | (behind 2) | (even) | 2000-01-01 origin/master |

  # print this usage message
  $ git-branch-status -h
  $ git-branch-status --help
      "prints this usage message"

  # show all remote branches - even those with no local
  $ git-branch-status -r
  $ git-branch-status --remotes
    | master         | (behind 1) | (even) | a-remote/master           |
    | (no local)     | n/a        | n/a    | a-remote/untracked-branch |

  # show all branches with timestamps (like -a -d)
  $ git-branch-status -v
  $ git-branch-status --verbose
    | 1999-12-31 local   | n/a        | n/a    | (no upstream)             |
    | 1999-12-31 master  | (behind 1) | (even) | 2000-01-01 origin/master  |
    | 1999-12-31 tracked | (even)     | (even) | 2000-01-01 origin/tracked |
USAGE


### constants ###

readonly MAX_COL_W=27 # should be => 12
readonly CWHITE='\033[0;37m'
readonly CGREEN='\033[0;32m'
readonly CYELLOW='\033[1;33m'
readonly CRED='\033[0;31m'
readonly CEND='\033[0m'
readonly CDEFAULT=$CWHITE
readonly CAHEAD=$CYELLOW
readonly CBEHIND=$CRED
readonly CEVEN=$CGREEN
readonly CNOUPSTREAM=$CRED
readonly CNOLOCAL=$CRED
readonly HRULE_CHAR='-'
readonly JOIN_CHAR='_'
readonly JOIN_REGEX="s/$JOIN_CHAR/ /"
readonly STAR="*"
readonly DELIM="|"
readonly NO_UPSTREAM="(no${JOIN_CHAR}upstream)"
readonly NO_LOCAL="(no${JOIN_CHAR}local)"
readonly NO_RESULTS_MSG="Everything is synchronized"


### variables ###
n_total_differences=0
local_w=0
behind_w=0
ahead_w=0
remote_w=0
declare -a local_msgs=()
declare -a behind_msgs=()
declare -a ahead_msgs=()
declare -a remote_msgs=()
declare -a local_colors=()
declare -a behind_colors=()
declare -a ahead_colors=()
declare -a remote_colors=()


### helpers ###

function get_refs # (a_refs_dir)
{
  git for-each-ref --format="%(refname:short) %(upstream:short)" $1 2> /dev/null
}

function get_status
{
  git rev-list --left-right ${local}...${remote} -- 2>/dev/null
}

function current_branch
{
  git rev-parse --abbrev-ref HEAD
}

function is_current_branch # (a_branch_name)
{
  current=$(current_branch)
  if (($SHOW_DATES))
  then this_branch=$(get_head_date $1)$1 ; current=$(get_head_date $current)$current ;
  else this_branch=$1
  fi

  if [ "$this_branch" == "$current" ] ; then echo 1 ; else echo 0 ; fi ;
}

function does_branch_exist # (a_branch_name)
{
  is_known_branch=$(git branch | grep -G "^  $1$") # all but current
  [ $(is_current_branch $1) -o "$is_known_branch" ] && echo 1 || echo 0
}

function set_filter_or_die # (a_branch_name)
{
  if (($(does_branch_exist $1)))
  then branch=$1
  else echo "no such branch: '$1'" ; exit ;
  fi
}

function get_head_date # (a_commit_ref)
{
  author_date=$(git log -n 1 --format=format:"%ai" $1 2> /dev/null)
  (($SHOW_DATES)) && [ "$author_date" ] && echo "${author_date:0:10}$JOIN_CHAR"
}

function get_commit_msg # (a_commit_ref)
{
  git log -n 1 --format=format:"%s" $1
}

function printHRule # (rule_w)
{
  printf "  $(head -c $1 < /dev/zero | tr '\0' $HRULE_CHAR)\n"
}


### business ###

function reset
{
  n_total_differences=0
  local_w=0
  behind_w=0
  ahead_w=0
  remote_w=0
  local_msgs=()
  behind_msgs=()
  ahead_msgs=()
  remote_msgs=()
  local_colors=()
  behind_colors=()
  ahead_colors=()
  remote_colors=()
}

function report # (a_local_branch_name a_remote_branch_name)
{
  local=$1
  remote=$2
  does_local_exist=$(does_branch_exist $local_branch)

  # filter branches per CLI arg
  [ $branch ] && [ "$branch" != "$local" ] && continue

  # filter heads
  [ "$local" == "HEAD" ] && continue

  # parse local<->remote sync status
  if (($does_local_exist)) && [ $remote ] ; then
    status=$(get_status) ; (($?)) && continue ;

    n_behind=$(echo $status | tr " " "\n" | grep -c '^>')
    n_ahead=$( echo $status | tr " " "\n" | grep -c '^<')
    n_differences=$(($n_behind + $n_ahead))
    n_total_differences=$(($n_total_differences + $n_differences))

    # filter branches by status
    (($SHOW_ALL_UPSTREAM)) || (($n_differences)) || continue

    # set data for branches with upstream
    local_color=$CDEFAULT
    if (($n_behind))
    then behind_msg="(behind$JOIN_CHAR$n_behind)" ; behind_color=$CBEHIND
    else behind_msg="(even)" ;                      behind_color=$CEVEN ;
    fi
    if (($n_ahead))
    then ahead_msg="(ahead$JOIN_CHAR$n_ahead)" ; ahead_color=$CAHEAD ;
    else ahead_msg="(even)" ;                    ahead_color=$CEVEN ;
    fi
    remote_color=$CDEFAULT
  elif (($does_local_exist)) && [ -z $remote ] && (($SHOW_ALL_LOCAL)) ; then
    # dummy data for local branches with no upstream counterpart
    local_color=$CDEFAULT
    behind_color="$CDEFAULT" ;  behind_msg="n/a" ;
    ahead_color="$CDEFAULT" ;   ahead_msg="n/a" ;
    remote_color=$CNOUPSTREAM ; remote="$NO_UPSTREAM" ;
  elif ! (($does_local_exist)) && [ $remote ] && (($SHOW_ALL_REMOTE)) ; then
    # dummy data for remote branches with no local counterpart
    local_color=$CNOLOCAL ; local="$NO_LOCAL" ;
    behind_color="$CDEFAULT" ; behind_msg="n/a" ;
    ahead_color="$CDEFAULT" ;  ahead_msg="n/a" ;
    remote_color=$CDEFAULT
  else continue
  fi

  # populate lists
  local_msg="$(get_head_date $local)$local" ;    local_msg="${local_msg:0:$MAX_COL_W}" ;
  remote_msg="$(get_head_date $remote)$remote" ; remote_msg="${remote_msg:0:$MAX_COL_W}" ;
  local_msgs=(    ${local_msgs[@]}    "$local_msg"    )
  behind_msgs=(   ${behind_msgs[@]}   "$behind_msg"   )
  ahead_msgs=(    ${ahead_msgs[@]}    "$ahead_msg"    )
  remote_msgs=(   ${remote_msgs[@]}   "$remote_msg"   )
  local_colors=(  ${local_colors[@]}  "$local_color"  )
  behind_colors=( ${behind_colors[@]} "$behind_color" )
  ahead_colors=(  ${ahead_colors[@]}  "$ahead_color"  )
  remote_colors=( ${remote_colors[@]} "$remote_color" )

  # determine max column widths
  if [ ${#local_msg}  -gt $local_w  ] ; then local_w=${#local_msg} ;   fi ;
  if [ ${#behind_msg} -gt $behind_w ] ; then behind_w=${#behind_msg} ; fi ;
  if [ ${#ahead_msg}  -gt $ahead_w  ] ; then ahead_w=${#ahead_msg} ;   fi ;
  if [ ${#remote_msg} -gt $remote_w ] ; then remote_w=${#remote_msg} ; fi ;
}

function printReportLine
{
  # fetch data
  local_msg=$(  echo ${local_msgs[$result_n]}  | sed "$JOIN_REGEX" )
  behind_msg=$( echo ${behind_msgs[$result_n]} | sed "$JOIN_REGEX" )
  ahead_msg=$(  echo ${ahead_msgs[$result_n]}  | sed "$JOIN_REGEX" )
  remote_msg=$( echo ${remote_msgs[$result_n]} | sed "$JOIN_REGEX" )
  local_color="${local_colors[$result_n]}"
  behind_color="${behind_colors[$result_n]}"
  ahead_color="${ahead_colors[$result_n]}"
  remote_color="${remote_colors[$result_n]}"

  # calculate column offsets
  local_offset=1
  behind_offset=$(( $local_w  - ${#local_msg}  ))
  ahead_offset=$((  $behind_w - ${#behind_msg} ))
  remote_offset=$(( $ahead_w  - ${#ahead_msg}  ))
  end_offset=$((    $remote_w - ${#remote_msg} ))

  # build output messages and display
  if (($(is_current_branch $local_msg))) ; then star=$STAR ; else star=" " ; fi ;
  local_msg="%$((  $local_offset  ))s$star$(echo -e $DELIM $local_color$local_msg$CEND)"
  behind_msg="%$(( $behind_offset ))s $(    echo -e $DELIM $behind_color$behind_msg$CEND)"
  ahead_msg="%$((  $ahead_offset  ))s $(    echo -e $DELIM $ahead_color$ahead_msg$CEND)"
  remote_msg="%$(( $remote_offset ))s $(    echo -e $DELIM $remote_color$remote_msg$CEND)"
  end_msg="%$((    $end_offset    ))s $DELIM"
  printf "$local_msg$behind_msg$ahead_msg$remote_msg$end_msg\n"
}

function printReport # (header)
{
  n_notable_differences=${#local_msgs[@]}

  # pretty print results
  printf "\n  $1\n"
  if [ "$n_notable_differences" != "0" ]
  then rule_w=$(($local_w+$behind_w+$ahead_w+$remote_w+13))
       printHRule $rule_w
       for (( result_n = 0 ; result_n < $n_notable_differences ; result_n++ ))
       do printReportLine
       done
       printHRule $rule_w
  fi

  # print something if no diffs
  if [ "$n_total_differences" == "0" -a "$(get_refs)" ]
  then rule_w=$((${#NO_RESULTS_MSG}+4))
       printHRule $rule_w
       echo -e "  $DELIM $CEVEN$NO_RESULTS_MSG$CEND $DELIM"
       printHRule $rule_w
  fi

  reset
}


### main entry ###

# parse CLI switches
if [ $1 ] ; then
  show_dates=0
  show_all=0
  show_all_local=0
  show_all_upstream=0
  show_all_remote=0
  if   [ "$1" == "-a" -o "$1" == "--all"     ] ; then show_all=1 ;
  elif [ "$1" == "-b" -o "$1" == "--branch"  ] ; then
    if [ $2 ] ; then set_filter_or_die $2 ; else branch=$(current_branch) ; fi ;
  elif [ "$1" == "-d" -o "$1" == "--dates"   ] ; then show_dates=1 ;
  elif [ "$1" == "-h" -o "$1" == "--help"    ] ; then echo "$USAGE" ; exit ;
  elif [ "$1" == "-r" -o "$1" == "--remotes" ] ; then show_all_remote=1 ;
  elif [ "$1" == "-v" -o "$1" == "--verbose" ] ; then show_all=1 ;
                                                      show_dates=1 ;
  else set_filter_or_die $1
  fi
  readonly SHOW_DATES=$show_dates
  readonly SHOW_ALL=$show_all
  readonly SHOW_ALL_LOCAL=$(($show_all    + $show_all_local))    # also show branches that have no upstream
  readonly SHOW_ALL_UPSTREAM=$(($show_all + $show_all_upstream)) # also show branches that are up to date
  readonly SHOW_ALL_REMOTE=$(($show_all   + $show_all_remote))   # also show branches that have no local
fi


# compare local branches status to their upstreams
while read local upstream ; do report $local $upstream ; done < <(get_refs refs/heads) ;
printReport "local <-> upstream"


(($SHOW_ALL_REMOTE)) || exit


# compare other remote branches status to local branches
for remote_repo in `git remote`
do
  while read remote_branch ; do
    local_branch=${remote_branch#$remote_repo/}
    upstream_branch=`git rev-parse --abbrev-ref $local_branch@{upstream} 2> /dev/null`

    [ "$remote_branch" != "$upstream_branch" ] && report $local_branch $remote_branch
  done < <(get_refs refs/remotes/$remote_repo)

  printReport "local <-> $remote_repo"
done
