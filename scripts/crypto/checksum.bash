#!/usr/bin/env bash
#==============================================================================
#  GLOBAL VARIABLES

NAME="MY-CHECKSUM"


#==============================================================================
#  H E L P E R S

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

info () {
  printf "[ ${BOLD}${BLUE}>>${RESET} ][${BOLD}management${RESET}] $1
"
}

success () {
  printf "[ ${BOLD}${GREEN}OK${RESET} ][${BOLD}management${RESET}] $1
"
}

warning () {
  printf "[ ${BOLD}${YELLOW}!!${RESET} ][${BOLD}management${RESET}] $1
"
}

error () {
  printf "[${BOLD}${RED}!!!!${RESET}][${BOLD}management${RESET}] $1
"
}


#==============================================================================
#  P A R A M E T E R   P A R S I N G

HELP=true
METHOD=false
CHECKSUM=false
FILE=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help|-h|--help)
    shift
    ;;
  -m|--method)
    shift
    METHOD=$1
    HELP=false
    shift
    ;;
  -c|--checksum)
    shift
    CHECKSUM=$1
    HELP=false
    shift
    ;;
  -f|--file)
    shift
    FILE=$1
    HELP=false
    shift
    ;;
  *)
    warning "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done

if [ ${METHOD} == false ] || [ ${CHECKSUM} == false ] || [ ${FILE} == false ]; then
  echo "Missing argumnets. All parameters have to be provided!"
  HELP=true
fi

#==============================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]; then
  echo ""
  echo "  ${BOLD}${NAME}${RESET} usage"
  echo
  echo "    ${BOLD}my-checksum ${GREEN}--method <checksum-calculator>${RESET} ${BOLD}${BLUE}--checksum <known-hash>${RESET} ${BOLD}${YELLOW}--file <my-file>${RESET}"
  echo ""
  echo "  Universal and user friendly checksum calculator. It can be used to verify a known checksum"
  echo "  of a given file."
  echo
  echo "    [${BOLD}help${RESET}|${BOLD}h${RESET}]                                 - Prints out this help text."
  echo "    ${BOLD}(-m|--method) <hash calculator command>${RESET}  - Command to calculate the hash."
  echo "    ${BOLD}(-c|--checksum) <known hash>${RESET}             - Known hash."
  echo "    ${BOLD}(-f|--file) <file path>${RESET}                  - File path to check."
  echo ""
  echo "  Available checksum calculators:"
  echo ""
  for tool in $(ls -1 --color=never /bin/ | grep -E '.+sum$'); do
    echo "    ${BOLD}$tool${RESET}"
  done
  echo ""
  exit 0
fi

echo
echo "Calculating checksum.."
echo
echo "Method:     ${BOLD}${METHOD}${RESET}"
echo "File:       ${BOLD}${FILE}${RESET}"
echo "Known:      ${BOLD}${CHECKSUM}${RESET}"

CALCULATED=$(${METHOD} ${FILE} | cut -d' ' -f1)

echo "Calculated: ${BOLD}${CALCULATED}${RESET}"

if [ ${CHECKSUM} = ${CALCULATED} ]; then
  echo
  echo "Result:     ${BOLD}${GREEN}MATCH${RESET}"
else
  echo "$(python3 -c "exec(\"import sys\na='${CHECKSUM}'\nb='${CALCULATED}'\nif len(a) != len(b):\n sys.exit()\np=list(' '*len(a))\ni=[p.__setitem__(j, '^') for j in range(len(a)) if a[j] != b[j]]\nprint(' '*12+''.join(p))\") ")"
  echo "Result:     ${BOLD}${RED}MISMATCH${RESET}"
fi

