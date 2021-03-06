#! /usr/bin/env python3
import argparse
import os
import sys


PATH = '~/.dotfiles/bin'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('type', choices=['bash', 'python'], help='template type')
    parser.add_argument('name', help='tool name without the "my-" prefix')
    args = parser.parse_args()

    name = args.name if args.name.startswith('my-') else 'my-' + args.name
    path = os.path.join(os.path.expanduser(PATH), name)
    if os.path.isfile(path):
        print('File already exists: {}'.format(path))
        sys.exit(-1)

    if args.type == 'bash':
        print('Bash tool template created to ', end='')
        content = BASH_TEMPLATE

    with open(path, 'w') as f:
        f.write(content)
        os.chmod(path, 0o755)

    print(path)



BASH_TEMPLATE = '''\
#! /bin/bash

#==============================================================================
#  GLOBAL VARIABLES

NAME="MY TOOL"


#==============================================================================
#  H E L P E R S

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

info () {
  printf "[ ${BOLD}${BLUE}>>${RESET} ][${BOLD}management${RESET}] $1\n"
}

success () {
  printf "[ ${BOLD}${GREEN}OK${RESET} ][${BOLD}management${RESET}] $1\n"
}

warning () {
  printf "[ ${BOLD}${YELLOW}!!${RESET} ][${BOLD}management${RESET}] $1\n"
}

error () {
  printf "[${BOLD}${RED}!!!!${RESET}][${BOLD}management${RESET}] $1\n"
}


#==============================================================================
#  P A R A M E T E R   P A R S I N G

HELP=true
PARAM=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help|-h|--help)
    shift
    ;;
  param)
    PARAM=true
    HELP=false
    shift
    ;;
  *)
    warning "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done


#==============================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]; then
  echo ""
  echo "  ${BOLD}${NAME}${RESET} usage"
  echo
  echo "  Description about the tool usage.."
  echo
  echo "    [${BOLD}help${RESET}|${BOLD}h${RESET}]   - Prints out this help text."
  echo "    ${BOLD}param${RESET}      - Some paramter."
  echo ""
  exit 0
fi


#==============================================================================
#  P A R A M   C O M M A N D

if [ $PARAM == true ]; then

  info "some progress"
  success "ok"

  exit 0
fi
'''

if __name__ == '__main__':
    main()

