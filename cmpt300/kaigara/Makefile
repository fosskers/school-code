TARGET=kaigara
CFLAGS=-Wall -g -fdiagnostics-color #-DNDEBUG
HEADERS=dbg.h
OBJECTS=

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	gcc $(CFLAGS) -c $< -o $@

kaigara: $(OBJECTS)
	gcc $(OBJECTS) $(CFLAGS) -o $@

clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)
