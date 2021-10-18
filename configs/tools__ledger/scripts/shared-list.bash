#!/usr/bin/env bash

set e

GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)
BOLD=$(tput bold)

third_text=$(ledger register tag shared and tag third and not tag done -X HUF --no-color --register-format "%d %50.50P %12t %12T\n")
third_amount=$(ledger register tag shared and tag third and not tag done -X HUF --register-format "%T\n" | tail -n 1 | cut -d' ' -f1 | tr -d ',' | tr -d '-')
third_amount=$(echo "$third_amount / 3" | bc)

full_text=$(ledger register tag shared and tag full and not tag done -X HUF --no-color --register-format "%d %50.50P %12t %12T\n")
full_amount=$(ledger register tag shared and tag full and not tag done -X HUF --register-format "%T\n" | tail -n 1 | cut -d' ' -f1 | tr -d ',' | tr -d '-')

final_amount=$(echo "$third_amount + $full_amount" | bc)

echo ""
echo "                                  ${BOLD}${YELLOW}H A R M A D O L T${RESET}"
echo "${BOLD}======================================================================================${RESET}"
echo "${BLUE}${third_text}${RESET}"
echo "--------------------------------------------------------------------------------------"
echo "${BOLD}${BLUE}$(printf "%'82d HUF\n" $third_amount)${RESET}"
echo ""
echo "                                    ${BOLD}${YELLOW}T E L J E S${RESET}"
echo "${BOLD}======================================================================================${RESET}"
echo "${BLUE}${full_text}${RESET}"
echo "--------------------------------------------------------------------------------------"
echo "${BOLD}${BLUE}$(printf "%'82d HUF\n" $full_amount)${RESET}"
echo ""
echo "                                  ${BOLD}${YELLOW}O S S Z E S E N${RESET}"
echo "${BOLD}======================================================================================${RESET}"
echo "${BOLD}${BLUE}$(printf "%'82d HUF\n" $third_amount)${RESET}"
echo "${BOLD}${BLUE}$(printf "%'82d HUF\n" $full_amount)${RESET}"
echo "--------------------------------------------------------------------------------------"
echo "${BOLD}${GREEN}$(printf "%'82d HUF\n" $final_amount)${RESET}"
