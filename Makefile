TARGET_EXEC=keyboard

CC=gcc
CXX=g++

LDFLAGS=-lstdc++

BUILD_DIR = ./build
SRC_DIRS = ./src

#ADD INC DIRS TO THIS
CFLAGS=-I./inc

SRCS = $(shell find $(SRC_DIRS) -name '*.cpp' -or -name '*.c' -or -name '*.s')

OBJS = $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@ $(LDFLAGS)

$(BUILD_DIR)/%.cpp.o: %.cpp
	mkdir -p $(dir $@)
	$(CXX) $(CFLAGS) -c $< -o $@ $(LDFLAGS)


