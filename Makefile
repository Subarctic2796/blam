CC = gcc
CFLAGS = -Wall -Wextra -Wswitch-enum -g

SRC = src
SRCS = $(wildcard $(SRC)/*.c)

OBJ = obj
OBJS = $(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(SRCS))

LIBS = -lreadline -lm

BIN = blam

.PHONY: all build clean run release

all: build

release: CFLAGS = -Wall -Wextra -Wswitch-enum -Werror -O2 -DNDEBUG
release: clean
release: build

build: $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LIBS)

$(OBJ)/%.o: $(SRC)/%.c | $(OBJ)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ):
	[[ -d $(OBJ) ]] || mkdir $(OBJ)

clean:
	$(RM) -r $(BIN) ./$(OBJ)

run: build
	@./$(BIN)
