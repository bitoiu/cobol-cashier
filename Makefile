# COBOL Bank Ledger System Makefile
# Uses GnuCOBOL (cobc) compiler

# Compiler and flags
CC = cobc
CFLAGS = -Wall -std=cobol2014 -free -I./copybooks
LDFLAGS =

# Directories
SRC_DIR = src
COPY_DIR = copybooks
DATA_DIR = data
BIN_DIR = bin
TEST_DIR = tests

# Source files
SOURCES = $(wildcard $(SRC_DIR)/*.cob)
COPYBOOKS = $(wildcard $(COPY_DIR)/*.cpy)

# Target executable
TARGET = $(BIN_DIR)/bankled
MAIN_SOURCE = $(SRC_DIR)/BANKLED.cob

# Default target
all: build

# Create directories
dirs:
	@mkdir -p $(BIN_DIR)
	@mkdir -p $(DATA_DIR)

# Build the main application
build: dirs $(TARGET)

$(TARGET): $(MAIN_SOURCE) $(COPYBOOKS)
	@echo "Compiling COBOL Bank Ledger System..."
	$(CC) $(CFLAGS) -x -o $(TARGET) $(MAIN_SOURCE) $(LDFLAGS)
	@echo "Build completed successfully!"

# Run the application
run: build
	@echo "Starting Bank Ledger System..."
	./$(TARGET)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(BIN_DIR)
	rm -f $(SRC_DIR)/*.o
	rm -f $(SRC_DIR)/*.so
	rm -f $(SRC_DIR)/*.c
	@echo "Clean completed!"

# Initialize data files
init-data: dirs
	@echo "Initializing data files..."
	@touch $(DATA_DIR)/accounts.dat
	@touch $(DATA_DIR)/trans.dat
	@echo "Data files initialized!"

# Reset database to baseline state with sample accounts
reset: clean
	@echo "Resetting database to baseline state..."
	@./scripts/reset-db.sh

# Install dependencies (GnuCOBOL)
install-deps:
	@echo "Installing GnuCOBOL dependencies..."
	@which brew > /dev/null || (echo "Homebrew not found. Please install Homebrew first." && exit 1)
	brew install gnu-cobol
	brew install gmp berkeley-db
	@echo "Dependencies installed!"

# Check if GnuCOBOL is installed
check-deps:
	@echo "Checking GnuCOBOL installation..."
	@which cobc > /dev/null || (echo "GnuCOBOL not found. Run 'make install-deps' first." && exit 1)
	@cobc --version
	@echo "GnuCOBOL is properly installed!"

# Build with debug information
debug: CFLAGS += -g -debug
debug: build

# Build and run tests
# Note: Current tests don't access the database, so no reset needed
test: build
	@echo "Building tests..."
	@$(CC) $(CFLAGS) -Wno-constant-expression -x -o $(BIN_DIR)/test-basic $(TEST_DIR)/test-basic.cob
	@echo "Running tests..."
	@./$(BIN_DIR)/test-basic

# Show help
help:
	@echo "COBOL Bank Ledger System - Makefile Help"
	@echo "========================================"
	@echo "Available targets:"
	@echo "  build      - Compile the application"
	@echo "  run        - Build and run the application"
	@echo "  clean      - Remove build artifacts"
	@echo "  reset      - Reset database to baseline state with sample data"
	@echo "  init-data  - Initialize data files"
	@echo "  install-deps - Install GnuCOBOL dependencies"
	@echo "  check-deps - Check if dependencies are installed"
	@echo "  debug      - Build with debug information"
	@echo "  test       - Build and run tests"
	@echo "  help       - Show this help message"
	@echo ""
	@echo "Usage examples:"
	@echo "  make build"
	@echo "  make run"
	@echo "  make clean && make build"

# Phony targets
.PHONY: all build run clean reset init-data install-deps check-deps debug test help dirs 