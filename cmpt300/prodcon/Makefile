TARGET=foundry
WARNINGS=-Wall -Wshadow -Wunreachable-code
CFLAGS=$(WARNINGS) -g -O -fdiagnostics-color -DNDEBUG
LDFLAGS=-lncurses
HEADERS=colour.h dbg.h lib/bstrlib.h
OBJECTS=foundry.o lib/bstrlib.o

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	gcc $(CFLAGS) -c $< -o $@

foundary: $(OBJECTS)
	gcc $(OBJECTS) $(CFLAGS) $(LDFLAGS) -o $@

clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)

# Compile Check
cc:
	make clean
	make all
