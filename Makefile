#=======================================================================================
# MAKE   SETTINGS

.DEFAULT_GOAL := help
SHELL := /bin/bash
NAME := DOTFILES


#=======================================================================================
# HELPER VARIABLES

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


#=======================================================================================
# HELP COMMAND

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


#=======================================================================================
# DEPLOYMENT COMMANDS

.PHONY: install
install:
	@./utils/install-scripts.bash
	@./utils/install-configs.bash

.PHONY: install-scripts
install-scripts:
	@./utils/install-scripts.bash


include ./utils/Makefile.targets

.PHONY: generate-targets
generate-targets:
	@./utils/generate-targets.bash

#=======================================================================================
# TEST COMMAND

.PHONY: test
test:
	@./tests/run.bash
