CC = gcc

CFLAGS = -g -Wall -std=c99 -fsanitize=address,undefined -pthread



TARGET_SERVER = rpsd
TARGET_CLIENT = rc 

SERVER_SRCS = rpsd.c network.c
SERVER_OBJS = $(SERVER_SRCS:.c=.o)

CLIENT_SRCS = rc.c network.c
CLIENT_OBJS = $(CLIENT_SRCS:.c=.o)


all: $(TARGET_SERVER) $(TARGET_CLIENT)


$(TARGET_SERVER): $(SERVER_OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)


$(TARGET_CLIENT): $(CLIENT_OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)


%.o: %.c network.h
	$(CC) $(CFLAGS) -c $< -o $@



clean:
	rm -f $(TARGET_SERVER) $(TARGET_CLIENT) $(SERVER_OBJS) $(CLIENT_OBJS) core.*


.PHONY: all clean