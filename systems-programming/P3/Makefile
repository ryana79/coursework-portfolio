CC = gcc
CFLAGS = -Wall -Wextra -g
TARGET = mysh
SRCS = mysh.c

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRCS)

clean:
	rm -f $(TARGET)
