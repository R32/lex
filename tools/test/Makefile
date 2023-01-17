#
CC       := gcc
INCLUDES := -I../../include
TARGET   := test.exe
OBJS     := test.o rstream.o

all: $(TARGET)
	@./$<

clean:
	@rm -f $(TARGET) $(OBJS)

.PHONY: clean all

$(TARGET): $(OBJS)
	$(CC) $^ -o $@

# lower-case vpath
vpath %.h ../../include
vpath %.c ../../include

test.o: test.c ../clex.template rstream.c rstream.h rlex.h
	$(CC) $(INCLUDES) -c $< -o $@

rstream.o: rstream.c rstream.h rlex.h
	$(CC) $(INCLUDES) -c $< -o $@

test.c: test.lex ../clex.template ../CLexer.hx rstream.c rstream.h rlex.h
	haxelib run lex $<