
SHELL:=/bin/bash

.PHONY: test
test:
	@cd tests; bats -r .

.PHONY: install
install:
	@mkdir -p ~/.scripts

.PHONY: deploy
deploy:
	@./utils/deploy.bash
