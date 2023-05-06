CC=g++
FLAGS=-g -Wall -Werror -Wextra
INC=includes/
BIN=bin
LIBS=-lreadline

step0_repl: step0_repl.cpp lineedit
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step0_repl.cpp $(BIN)/lineedit.o -o $(BIN)/step0_repl

step1_read_print: step1_read_print.cpp lineedit reader printer types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step1_read_print.cpp \
$(BIN)/lineedit.o $(BIN)/reader.o $(BIN)/types.o $(BIN)/printer.o \
-o $(BIN)/step1_read_print

printer: printer.cpp $(INC)/printer.h types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c printer.cpp -o $(BIN)/printer.o

reader: reader.cpp $(INC)/reader.h lineedit types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c reader.cpp -o $(BIN)/reader.o

types: types.cpp $(INC)/types.h lineedit
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c types.cpp -o $(BIN)/types.o

lineedit: lineedit.cpp $(INC)/lineedit.h
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c lineedit.cpp -o $(BIN)/lineedit.o