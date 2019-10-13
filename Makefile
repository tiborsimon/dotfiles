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
	@echo ""
	@echo "$(BOLD)$(BLUE)-------------------------------------------------------------------$(RESET)"
	@echo "  $(BOLD) $(BOLD_NAME) make interface $(RESET)"
	@echo "$(BOLD)$(BLUE)-------------------------------------------------------------------$(RESET)"
	@echo ""
	@echo "   $(BOLD)$(BLUE)help$(RESET)              Prints out this help message."
	@echo ""
	@echo "   $(BOLD)$(GREEN)install$(RESET)           Installs all configurations."
	@echo "   $(BOLD)$(GREEN)install-*$(RESET)         Installs the given configuration."
	@echo ""
	@echo "   $(BOLD)$(BLUE)generate-targets$(RESET)  Regenerates the config target list.."
	@echo ""
	@echo "   $(BOLD)$(YELLOW)test$(RESET)              Runs the test suite."
	@echo ""


# =======================================================================================
#  D E P L O Y M E N T   C O M M A N D S

.PHONY: install
install:
	@./utils/install-scripts.bash
	@./utils/install-configs.bash

# include ./utils/Makefile.targets

.PHONY: generate-targets
generate-targets:
	@./utils/generate-targets.bash

# =======================================================================================
#  T E S T   C O M M A N D

.PHONY: test
test:
	@./tests/run.bash
