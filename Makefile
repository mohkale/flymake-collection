SRC   := lisp/*.el
EMACS ?= emacs --eval '(add-to-list (quote load-path) (concat default-directory "lisp/"))'

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: ## Check for missing or poorly formatted docstrings
	@for file in $(SRC); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: compile
compile: ## Check for byte-compiler errors
	@for file in $(SRC); do \
	    echo "[compile] $$file" ;\
	    rm -f "$${file}c" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: test
test: ## Run all defined test cases.
	@echo "[test] Running all test cases"
	@docker build  -t flymake-collection-test ./tests/checkers
	@docker run \
	  --rm \
	  --volume "$$(pwd)":/src:ro \
	  --volume "$$(pwd)/tests/checkers":/test:ro \
	  flymake-collection-test \
	  sh -c 'find /test/test-cases/ -iname \*.yml | parallel -I{} chronic /test/run-test-case {}'

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" $(subst .el,.elc,$(SRC))
	@rm -f $(subst .el,.elc,$(SRC))
