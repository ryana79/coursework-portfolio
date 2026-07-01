CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -O2
TARGET = outlier
SRCS = outlier.c

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRCS)

clean:
	rm -f $(TARGET)
