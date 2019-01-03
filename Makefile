
SHELL:=/bin/bash

.PHONY: test
test:
	@cd tests; bats -r .

.PHONY: install
install:
	@./utils/install.bash

.PHONY: install-aur
install-aur:
	@./utils/install-aur.bash

.PHONY: deploy
deploy:
	@./utils/deploy.bash
