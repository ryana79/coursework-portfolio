CC = gcc
CFLAGS = -Wall -Werror -g

TARGETS = memtest memgrind test_mymalloc

all: $(TARGETS)

memtest: memtest.c mymalloc.c
	$(CC) $(CFLAGS) -o memtest memtest.c mymalloc.c

memgrind: memgrind.c mymalloc.c
	$(CC) $(CFLAGS) -o memgrind memgrind.c mymalloc.c

test_mymalloc: test_mymalloc.c mymalloc.c
	$(CC) $(CFLAGS) -o test_mymalloc test_mymalloc.c mymalloc.c

clean:
	rm -f $(TARGETS)
