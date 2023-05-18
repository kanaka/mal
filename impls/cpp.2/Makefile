CC=g++
FLAGS=-g -Wall -Werror -Wextra
INC=includes/
BIN=bin
LIBS=-lreadline -lgmpxx -lgmp

step0_repl: step0_repl.cpp lineedit
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step0_repl.cpp $(BIN)/lineedit.o -o $(BIN)/step0_repl

step1_read_print: step1_read_print.cpp lineedit reader printer types parse_numbers parse_sequences parse_reader_macros
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step1_read_print.cpp \
$(BIN)/lineedit.o $(BIN)/reader.o $(BIN)/types.o $(BIN)/printer.o \
$(BIN)/parse_numbers.o $(BIN)/parse_sequences.o $(BIN)/parse_reader_macros.o \
-o $(BIN)/step1_read_print

step2_eval: step2_eval.cpp lineedit reader printer types parse_numbers parse_sequences parse_reader_macros eval env apply
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step2_eval.cpp \
$(BIN)/lineedit.o $(BIN)/reader.o $(BIN)/types.o $(BIN)/printer.o \
$(BIN)/parse_numbers.o $(BIN)/parse_sequences.o $(BIN)/parse_reader_macros.o \
$(BIN)/eval.o $(BIN)/env.o $(BIN)/apply.o \
-o $(BIN)/step2_eval


step3_env: step2_eval.cpp lineedit reader printer types parse_numbers parse_sequences parse_reader_macros eval env apply
	$(CC) $(FLAGS) -I$(INC) $(LIBS) step3_env.cpp \
$(BIN)/lineedit.o $(BIN)/reader.o $(BIN)/types.o $(BIN)/printer.o \
$(BIN)/parse_numbers.o $(BIN)/parse_sequences.o $(BIN)/parse_reader_macros.o \
$(BIN)/eval.o $(BIN)/env.o $(BIN)/apply.o \
-o $(BIN)/step3_env



eval: eval.cpp types env apply
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c eval.cpp -o $(BIN)/eval.o

apply: apply.cpp types env
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c apply.cpp -o $(BIN)/apply.o
env: env.cpp types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c env.cpp -o $(BIN)/env.o


printer: printer.cpp $(INC)/printer.h types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c printer.cpp -o $(BIN)/printer.o


reader: reader.cpp $(INC)/reader.h lineedit types parse_numbers parse_sequences parse_reader_macros
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c reader.cpp -o $(BIN)/reader.o

parse_reader_macros: parse_reader_macros.cpp $(INC)/parse_reader_macros.h types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c parse_reader_macros.cpp -o $(BIN)/parse_reader_macros.o

parse_sequences: parse_sequences.cpp $(INC)/parse_sequences.h types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c parse_sequences.cpp -o $(BIN)/parse_sequences.o

parse_numbers: parse_numbers.cpp $(INC)/parse_numbers.h types
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c parse_numbers.cpp -o $(BIN)/parse_numbers.o

types: types.cpp $(INC)/types.h lineedit
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c types.cpp -o $(BIN)/types.o

lineedit: lineedit.cpp $(INC)/lineedit.h
	$(CC) $(FLAGS) -I$(INC) $(LIBS) -c lineedit.cpp -o $(BIN)/lineedit.o