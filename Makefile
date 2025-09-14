CXX = g++
CXXFLAGS = -g -Wall -Wextra -std=c++17

SRCDIR = src
BUILDDIR = build
TARGET = $(BUILDDIR)/mycc

SOURCES = $(wildcard $(SRCDIR)/*.cpp)
OBJECTS = $(patsubst $(SRCDIR)/%.cpp, $(BUILDDIR)/%.o, $(SOURCES))

.PHONY: all test clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	@mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJECTS)
	@echo "Compiler built successfully: $(TARGET)"

$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	@mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

test: $(TARGET)
	@echo "--- Running Lexer Test ---"
	@./$(TARGET) --lex compiler_test/test.c

clean:
	rm -rf $(BUILDDIR)
	@echo "Build directory cleaned."