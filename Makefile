TARGET_EXEC=keyboard

CC=gcc
CXX=g++

# Linking flags (What libraries to link to, etc)
LDFLAGS=-lstdc++ $(shell llvm-config --ldflags)
LIBS = $(shell llvm-config --libs)
BUILD_DIR = ./build
SRC_DIRS = ./src

#ADD INC DIRS TO THIS
#CFlags are where all the includes and flags for generating object files go to
CFLAGS=-I./inc $(shell llvm-config --cxxflags) -fexceptions
SRCS = $(shell find $(SRC_DIRS) -name '*.cpp' -or -name '*.c' -or -name '*.s')

OBJS = $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@ 

$(BUILD_DIR)/%.cpp.o: %.cpp
	mkdir -p $(dir $@)
	$(CXX) $(CFLAGS) -c $< -o $@


