#
CC       := gcc
INCLUDES := -I../../include
TARGET   := test.exe
OBJS     := test_lex.o test_slr.o rstream.o

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

test_lex.o: test_lex.c rstream.o
	$(CC) $(INCLUDES) -c $< -o $@

test_slr.o: test_slr.c rstream.o
	$(CC) $(INCLUDES) -c $< -o $@

rstream.o: rstream.c rstream.h rlex.h
	$(CC) $(INCLUDES) -c $< -o $@

test_lex.c test_slr.c: test.slr test.lex ../clex.template ../cslr.template
	haxelib run lex --slr test.slr test.lex
