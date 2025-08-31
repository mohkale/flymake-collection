SRC_DIR = src
BIN_DIR = bin

SRC      := $(wildcard $(SRC_DIR)/*.el $(SRC_DIR)/checkers/*.el)
ELC      := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.elc,$(SRC))
ELCHKDOC := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.checkdoc,$(SRC))

# Obsolete warnings suppressed for `rx-constituents'. The replacement
# does not let you define optional arguments for properties which makes
# it incompatbile with the regexp format we've used until now.
EMACS ?= emacs \
    --eval '(add-to-list (quote load-path) (concat default-directory "src/"))' \
	--eval '(byte-compile-disable-warning (quote obsolete))'

$(V).SILENT:

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: $(ELCHKDOC)

$(BIN_DIR)/%.checkdoc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[checkdoc] $^"
	$(EMACS) -Q --batch \
	    --eval "(or (fboundp 'checkdoc-file) (kill-emacs 1))" \
	    --eval "(setq sentence-end-double-space nil)" \
	    --eval "(checkdoc-file \"$^\")" 2>&1 \
		| tee "$@" \
	    | grep . && exit 1 || true


.PHONY: compile
compile: $(ELC)

$(BIN_DIR)/%.elc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[compile] $^"
	$(EMACS) -Q --batch -L . -f batch-byte-compile "$^" 2>&1 \
		| grep -v -e "^Wrote" -e "^Loading" \
		| grep . && exit 1 || true ;\
	mv -f "$^c" "$@"

.PHONY: docker-build
docker-build: ## Create a build image for running tests
	@echo "[docker] Building docker image"
	docker build -t flymake-collection-test --progress plain ./tests/checkers

DOCKER_FLAGS := -it
define docker_run
	docker run \
	  --rm \
	  $(DOCKER_FLAGS) \
      --workdir /workspaces/flymake-collection \
	  --volume "$$(pwd)":/workspaces/flymake-collection:ro \
	  flymake-collection-test \
	  $1 $2 $3 $4 $5 $6 $7 $8 $9
endef

DOCKER_RUN := bash
.PHONY: docker
docker: docker-build ## Run a command in the built docker-image.
	$(call docker_run,$(DOCKER_RUN))

.PHONY: docker-test
docker-test: ## Run tests from within the docker image
	$(call docker_run,make,test)

LINTERS := *
.PHONY: test
test: ## Run all defined test cases.
	@echo "[test] Running all test cases"
	find ./tests/checkers/test-cases/ -iname '$(LINTERS).yml' | parallel -I{} chronic ./tests/checkers/run-test-case {}

.PHONY: clean
clean: ## Remove build artifacts
	for file in $(ELC) $(ELCHKDOC); do \
	    if [ -e "$$file" ]; then \
			echo "[clean] $$file"; \
	        rm -f "$$file"; \
	    fi; \
	done
