# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

EXEC := expr-fix

SRC_DIR := src
OBJ_DIR := obj

SRCS := $(wildcard $(SRC_DIR)/*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

CC := gcc
CFLAGS := -Wall -Wextra -Wpedantic
INC := -I./dep/

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) -o $@ $(CFLAGS) -c $< $(INC)

$(OBJ_DIR):
	@mkdir -p $@

.PHONY: clean
clean:
	@rm -frv $(OBJ_DIR)

.PHONY: remove
remove:
	@rm -frv $(OBJ_DIR) $(EXEC)
