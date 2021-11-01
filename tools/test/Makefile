#
CC       := gcc
INCLUDES := -I../include
TARGET   := test.exe
OBJS     := test.o rstream.o

all: $(TARGET)
	@./$<

clean:
	$(TARGET) $(OBJS)

.PHONY: clean all

$(TARGET): $(OBJS)
	$(CC) $^ -o $@

# lower-case vpath
vpath %.h ../include
vpath %.c ../include

test.o: test.c rstream.c rstream.h rlex.h
	$(CC) $(INCLUDES) -c $< -o $@

rstream.o: rstream.c rstream.h rlex.h
	$(CC) $(INCLUDES) -c $< -o $@

test.c: test.lex
	haxelib run lex $<
