
SHELL:=/bin/bash

.PHONY: test
test:
	@cd tests; bats -r .

.PHONY: install
install:
	@./utils/install.bash

.PHONY: install-custom
install-custom:
	@./utils/install-custom.bash

.PHONY: deploy
deploy:
	@./utils/deploy.bash
