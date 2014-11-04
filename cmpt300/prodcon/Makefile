TARGET=foundry
WARNINGS=-Wall -Wshadow -Wunreachable-code
CFLAGS=$(WARNINGS) -g -O -fdiagnostics-color -DNDEBUG
LDFLAGS=-lncurses -pthread
HEADERS=colour.h dbg.h metals.h operator.h utils.h lib/bstrlib.h
OBJECTS=foundry.o utils.o metals.o operator.o lib/bstrlib.o

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	gcc $(CFLAGS) -c $< -o $@

foundry: $(OBJECTS)
	gcc $(OBJECTS) $(CFLAGS) $(LDFLAGS) -o $@

clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)

# Compile Check
cc:
	make clean
	make all
