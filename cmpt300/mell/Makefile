TARGET=mell
WARNINGS=-Wall -Wshadow -Wunreachable-code
CFLAGS=$(WARNINGS) -g -fdiagnostics-color -DNDEBUG
HEADERS=colour.h dbg.h defines.h lib/bstrlib.h
OBJECTS=mell.o lib/bstrlib.o

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	gcc $(CFLAGS) -c $< -o $@

mell: $(OBJECTS)
	gcc $(OBJECTS) $(CFLAGS) -o $@

clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)

# Compile Check
cc:
	make clean
	make all
