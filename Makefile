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

.PHONY: install-bash
install-bash:
	@./configs/install.bash bash

.PHONY: install-emacs
install-emacs:
	@./configs/install.bash emacs

.PHONY: install-fish
install-fish:
	@./configs/install.bash fish

.PHONY: install-git
install-git:
	@./configs/install.bash git

.PHONY: install-keyboard
install-keyboard:
	@./configs/install.bash keyboard

.PHONY: install-ledger
install-ledger:
	@./configs/install.bash ledger

.PHONY: install-moc
install-moc:
	@./configs/install.bash moc

.PHONY: install-profile
install-profile:
	@./configs/install.bash profile

.PHONY: install-pypi
install-pypi:
	@./configs/install.bash pypi

.PHONY: install-python
install-python:
	@./configs/install.bash python

.PHONY: install-ranger
install-ranger:
	@./configs/install.bash ranger

.PHONY: install-tmux
install-tmux:
	@./configs/install.bash tmux

.PHONY: install-vim
install-vim:
	@./configs/install.bash vim

# =======================================================================================
#  T E S T   C O M M A N D

.PHONY: test
test:
	@cd tests; bats -r .
