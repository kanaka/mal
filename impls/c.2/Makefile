CC = gcc

CFLAGS = -std=c99 -g -Wall

LIBS = -ledit -lgc
FFI_LIBS = -ldl -lffi

SRC = reader.c printer.c types.c env.c core.c
HEADERS = reader.h printer.h types.h env.h core.h

LIB_DIR = ./libs
LIB_LIST_H = $(LIB_DIR)/linked_list/linked_list.h
LIB_LIST_SRC = $(LIB_DIR)/linked_list/linked_list.c

LIB_MAP_H = $(LIB_DIR)/hashmap/hashmap.h
LIB_MAP_SRC = $(LIB_DIR)/hashmap/hashmap.c

LIBS_H = $(LIB_LIST_H) $(LIB_MAP_H)
LIBS_SRC = $(LIB_LIST_SRC) $(LIB_MAP_SRC)

S0_SRC = step0_repl.c
S1_SRC = step1_read_print.c reader.c types.c printer.c $(LIB_LIST_SRC)
S2_SRC = step2_eval.c reader.c types.c printer.c $(LIBS_SRC)
S3_SRC = step3_env.c reader.c types.c printer.c env.c $(LIBS_SRC)
S4_SRC = step4_if_fn_do.c $(SRC) $(LIBS_SRC)
S5_SRC = step5_tco.c $(SRC) $(LIBS_SRC)
S6_SRC = step6_file.c $(SRC) $(LIBS_SRC)
S7_SRC = step7_quote.c $(SRC) $(LIBS_SRC)
S8_SRC = step8_macros.c $(SRC) $(LIBS_SRC)
S9_SRC = step9_try.c $(SRC) $(LIBS_SRC)
SA_SRC = stepA_mal.c $(SRC) $(LIBS_SRC)

S0_HEADERS =
S1_HEADERS = reader.h types.h printer.h $(LIB_LIST_H)
S2_HEADERS = reader.h types.h printer.h $(LIBS_H)
S3_HEADERS = reader.h types.h printer.h env.h $(LIBS_H)
S4_HEADERS = $(HEADERS) $(LIBS_H)
S5_HEADERS = $(HEADERS) $(LIBS_H)
S6_HEADERS = $(HEADERS) $(LIBS_H)
S7_HEADERS = $(HEADERS) $(LIBS_H)
S8_HEADERS = $(HEADERS) $(LIBS_H)
S9_HEADERS = $(HEADERS) $(LIBS_H)
SA_HEADERS = $(HEADERS) $(LIBS_H)

S0 = step0_repl
S1 = step1_read_print
S2 = step2_eval
S3 = step3_env
S4 = step4_if_fn_do
S5 = step5_tco
S6 = step6_file
S7 = step7_quote
S8 = step8_macros
S9 = step9_try
SA = stepA_mal

all: $(S0) $(S1) $(S2) $(S3) $(S4) $(S5) $(S6) $(S7) $(S8) $(S9) $(SA)

$(S0): $(S0_SRC) $(S0_HEADERS)
	$(CC) $(CFLAGS) $(S0_SRC) $(LIBS) -o $(S0)

$(S1): $(S1_SRC) $(S1_HEADERS)
	$(CC) $(CFLAGS) $(S1_SRC) $(LIBS) -o $(S1)

$(S2): $(S2_SRC) $(S2_HEADERS)
	$(CC) $(CFLAGS) $(S2_SRC) $(LIBS) -o $(S2)

$(S3): $(S3_SRC) $(S3_HEADERS)
	$(CC) $(CFLAGS) $(S3_SRC) $(LIBS) -o $(S3)

$(S4): $(S4_SRC) $(S4_HEADERS)
	$(CC) $(CFLAGS) $(S4_SRC) $(LIBS) -o $(S4)

$(S5): $(S5_SRC) $(S5_HEADERS)
	$(CC) $(CFLAGS) $(S5_SRC) $(LIBS) -o $(S5)

$(S6): $(S6_SRC) $(S6_HEADERS)
	$(CC) $(CFLAGS) $(S6_SRC) $(LIBS) -o $(S6)

$(S7): $(S7_SRC) $(S7_HEADERS)
	$(CC) $(CFLAGS) $(S7_SRC) $(LIBS) -o $(S7)

$(S8): $(S8_SRC) $(S8_HEADERS)
	$(CC) $(CFLAGS) $(S8_SRC) $(LIBS) -o $(S8)

$(S9): $(S9_SRC) $(S9_HEADERS)
	$(CC) $(CFLAGS) $(S9_SRC) $(LIBS) -o $(S9)

$(SA): $(SA_SRC) $(SA_HEADERS)
	$(CC) $(CFLAGS) $(SA_SRC) $(LIBS) $(FFI_LIBS) -DWITH_FFI -o $(SA)

.PHONY clean:
	rm -f $(S0) $(S1) $(S2) $(S3) $(S4) $(S5) $(S6) $(S7) $(S8) $(S9) $(SA)
