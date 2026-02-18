.PHONY: help test testall testone document check install clean
TESTFILE ?= inst/tinytest/test_initialize.R

help: ## Display this help screen
	@printf "\033[1mAvailable commands:\033[0m\n\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

test: ## Build, install, and run all tinytest tests
	Rscript -e "tinytest::build_install_test(pkg='.')"

testall: test

testone: ## Run one tinytest file: make testone TESTFILE=inst/tinytest/test_initialize.R
	Rscript -e "pkgload::load_all('.'); tinytest::run_test_file('$(TESTFILE)')"

document: ## Generate Rd/NAMESPACE with roxygen2 and render README.md
	Rscript -e "devtools::document('.')"
	quarto render README.qmd --to gfm

check: document ## Run package checks
	Rscript -e "devtools::check('.')"

install: document ## Install the package (no dependency install)
	Rscript -e "devtools::install('.', dependencies = FALSE)"

clean: ## Remove common build/check artifacts
	rm -rf *.Rcheck
	rm -f *.tar.gz
