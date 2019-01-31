# =======================================================================================
#  M A K E   S E T T I N G S

.DEFAULT_GOAL := help
SHELL := /bin/bash
NAME := DOTFILES


# =======================================================================================
#  H E L P E R   V A R I A B L E S

BOLD   := $(shell tput bold)
RED    := $(shell tput setaf 1)
GREEN  := $(shell tput setaf 2)
YELLOW := $(shell tput setaf 3)
BLUE   := $(shell tput setaf 4)
RESET  := $(shell tput sgr0)

BOLD_NAME := $(BOLD)$(NAME)$(RESET)

TASK    := [ $(BOLD)$(GREEN)>>$(RESET) ]
OK      := [ $(BOLD)$(GREEN)OK$(RESET) ]
ASK     := [ $(BOLD)$(BLUE)??$(RESET) ]
WARNING := [ $(BOLD)$(YELLOW)!!$(RESET) ]
ERROR   := [$(BOLD)$(RED)FAIL$(RESET)]


# =======================================================================================
#  H E L P   C O M M A N D

.PHONY: help
help:
	@echo ""
	@echo "$(BOLD)$(BLUE)===================================================================$(RESET)"
	@echo "                $(BOLD)$(GREEN).o0 $(BLUE)~$(RESET) $(BOLD_NAME) make interface $(BOLD)$(BLUE)~ $(GREEN)0o.$(RESET)"
	@echo "$(BOLD)$(BLUE)===================================================================$(RESET)"
	@echo ""
	@echo "   $(BOLD)$(BLUE)help$(RESET)            Prints out this help message."
	@echo ""
	@echo "   $(BOLD)$(GREEN)all$(RESET)             Performs all commands at once."
	@echo ""
	@echo "   $(BOLD)$(YELLOW)install$(RESET)         Installs the dependencies."
	@echo "   $(BOLD)$(YELLOW)install-custom$(RESET)  Executes the custom dependency installations."
	@echo "   $(BOLD)$(YELLOW)deploy$(RESET)          Deploys the configuration files."
	@echo ""
	@echo "$(BOLD)$(BLUE)===================================================================$(RESET)"


# =======================================================================================
#  D E P L O Y M E N T   C O M M A N D S

.PHONY: install
install:
	@./scripts/install.bash
	@./configs/install.bash

.PHONY: install-scripts
install-scripts:
	@./scripts/install.bash


include ./configs/Makefile

.PHONY: generate-targets
generate-targets:
	@./configs/generate-targets.bash

# =======================================================================================
#  T E S T   C O M M A N D

.PHONY: test
test:
	@cd tests; bats -r .
