TARGET=kaigara
WARNINGS=-Wall -Wshadow -Wunreachable-code
CFLAGS=$(WARNINGS) -g -fdiagnostics-color #-DNDEBUG
HEADERS=dbg.h defines.h
OBJECTS=kaigara.o

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	gcc $(CFLAGS) -c $< -o $@

kaigara: $(OBJECTS)
	gcc $(OBJECTS) $(CFLAGS) -o $@

clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)

# Compile Check
cc:
	make clean
	make all
