# POSTGRES OR MYSQL
MODE = POSTGRES

SOURCES = init.sql types.sql reader.sql printer.sql env.sql step4_if_fn_do.sql
SOURCES_LISP = env.sql step4_if_fn_do.sql

#STEP0_DEPS = init.sql
#STEP1_DEPS = $(STEP0_DEPS) types.sql reader.sql printer.sql
#
#step%.sql: step%.sql.in
#	cpp -P -traditional-cpp -D $(MODE) $< $@
#
#step0_repl.sql: $(STEP0_DEPS)
#step1_read_print.sql: $(STEP1_DEPS)
#
#clean:
#	rm -f step*.sql
#
.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

