# Guile Scheme Category Theory Calculus Engine Makefile

# Project configuration
PROJECT_NAME ?= scheme-category-calculus
PROJECT_ROOT ?= $(shell pwd)

# Guile configuration
GUILE = guile3
GUILD = guild3
SCHEME_FILES = $(shell find . -name "*.scm" -not -path "./.cache/*")
EXAMPLES = $(wildcard examples/*.scm)

# Development environment
TMUX_SESSION = $(PROJECT_NAME)
EMACS_CONFIG = $(PROJECT_NAME).el

.PHONY: all check examples clean help dev dev-attach dev-stop dev-status

all: check examples

help:
	@echo "Guile Scheme Category Theory Calculus Engine"
	@echo "============================================"
	@echo ""
	@echo "Available targets:"
	@echo "  make check        - Run dependency check"
	@echo "  make examples     - Run all examples"
	@echo "  make clean        - Clean compiled files"
	@echo "  make dev          - Start development environment (tmux + Emacs)"
	@echo "  make dev-attach   - Attach to running development session"
	@echo "  make dev-stop     - Stop development session"
	@echo "  make dev-status   - Check development session status"
	@echo "  make help         - Show this help"

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

dev:
	@echo "Starting development environment for $(PROJECT_NAME)..."
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		echo "Session $(TMUX_SESSION) already exists. Attaching..."; \
		tmux attach-session -t $(TMUX_SESSION); \
	else \
		echo "Creating new tmux session: $(TMUX_SESSION)"; \
		tmux new-session -d -s $(TMUX_SESSION) "emacs -nw -Q -l $(EMACS_CONFIG)"; \
		echo "Session started. TTY: $$(tmux list-panes -t $(TMUX_SESSION) -F '#{pane_tty}')"; \
		echo "Run 'make dev-attach' to connect"; \
	fi

dev-attach:
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		tmux attach-session -t $(TMUX_SESSION); \
	else \
		echo "No session found. Run 'make dev' first."; \
		exit 1; \
	fi

dev-stop:
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		echo "Stopping session $(TMUX_SESSION)..."; \
		tmux kill-session -t $(TMUX_SESSION); \
	else \
		echo "Session $(TMUX_SESSION) is not running."; \
	fi

dev-status:
	@echo "Development Environment Status:"
	@echo "-------------------------------"
	@echo "Project: $(PROJECT_NAME)"
	@echo "Root: $(PROJECT_ROOT)"
	@if tmux has-session -t $(TMUX_SESSION) 2>/dev/null; then \
		echo "Session: RUNNING"; \
		echo "TTY: $$(tmux list-panes -t $(TMUX_SESSION) -F '#{pane_tty}')"; \
		echo "Windows: $$(tmux list-windows -t $(TMUX_SESSION) | wc -l)"; \
	else \
		echo "Session: NOT RUNNING"; \
	fi

.DEFAULT_GOAL := help