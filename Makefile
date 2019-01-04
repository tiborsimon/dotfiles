
SHELL:=/bin/bash

.PHONY: test
test:
	@cd tests; bats -r .

.PHONY: install
install:
	@./configs/install.bash

.PHONY: install-custom
install-custom:
	@./configs/install-custom.bash

.PHONY: deploy
deploy:
	@./scripts/deploy.bash
	@./configs/deploy.bash
