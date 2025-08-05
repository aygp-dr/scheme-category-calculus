# Guile Scheme Category Theory Calculus Engine Makefile

GUILE = guile3
GUILD = guild3
SCHEME_FILES = $(shell find . -name "*.scm" -not -path "./.cache/*")
EXAMPLES = $(wildcard examples/*.scm)

.PHONY: all check examples clean help

all: check examples

help:
	@echo "Guile Scheme Category Theory Calculus Engine"
	@echo "============================================"
	@echo ""
	@echo "Available targets:"
	@echo "  make check      - Run dependency check"
	@echo "  make examples   - Run all examples"
	@echo "  make clean      - Clean compiled files"
	@echo "  make help       - Show this help"

check:
	@echo "=== Running dependency check ===" 
	@$(GUILE) experiments/000-deps-check/check.scm

examples: check
	@echo ""
	@echo "=== Running examples ==="
	@for example in $(EXAMPLES); do \
		echo ""; \
		echo "--- Running $$example ---"; \
		$(GUILE) $$example || true; \
	done

clean:
	@echo "Cleaning compiled files..."
	@find . -name "*.go" -delete
	@rm -rf .cache/

.DEFAULT_GOAL := help