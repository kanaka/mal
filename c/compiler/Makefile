OS:=$(shell uname)
CC=gcc
CFLAGS=-Itinycc -Wall -Wextra -Werror -g
LDLIBS=-ledit -ltermcap -lgc -lpcre -ldl

ALL_STEPS=step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal malcc

.PHONY: all clean test test-current cloc docker-build

all: $(ALL_STEPS)

step0_repl: step0_repl.o
step1_read_print: step1_read_print.o hashmap.o printer.o reader.o types.o util.o
step2_eval: step2_eval.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step3_env: step3_env.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step4_if_fn_do: step4_if_fn_do.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step5_tco: step5_tco.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step6_file: step6_file.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step7_quote: step7_quote.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step8_macros: step8_macros.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
step9_try: step9_try.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
stepA_mal: stepA_mal.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a
malcc: malcc.o core.o env.o hashmap.o printer.o reader.o types.o util.o tinycc/libtcc.a

tinycc/libtcc.a:
	cd tinycc && ./configure && make

clean:
	rm -f $(ALL_STEPS) *.o
	cd tinycc && make clean

mal-in-mal: all
	cd mal/mal && ../../malcc --compile stepA_mal.mal ../../mal-in-mal

test: test0 test1 test2 test3 test4 test5 test6 test7 test8 test9 testA test-malcc test-self-hosted test-supplemental test-mal-in-mal

RUN_TEST_CMD=mal/runtest.py --rundir mal/tests --hard --deferrable --optional --start-timeout 1 --test-timeout 1

test0: all
	$(RUN_TEST_CMD) step0_repl.mal ../../step0_repl

test1: all
	$(RUN_TEST_CMD) step1_read_print.mal ../../step1_read_print

test2: all
	$(RUN_TEST_CMD) step2_eval.mal ../../step2_eval

test3: all
	$(RUN_TEST_CMD) step3_env.mal ../../step3_env

test4: all
	$(RUN_TEST_CMD) step4_if_fn_do.mal ../../step4_if_fn_do

test5: all
	$(RUN_TEST_CMD) step5_tco.mal ../../step5_tco

test6: all
	$(RUN_TEST_CMD) step6_file.mal ../../step6_file
	mal/run_argv_test.sh ./step6_file

test7: all
	$(RUN_TEST_CMD) step7_quote.mal ../../step7_quote

test8: all
	$(RUN_TEST_CMD) step8_macros.mal ../../step8_macros

test9: all
	$(RUN_TEST_CMD) step9_try.mal ../../step9_try

testA: all
	$(RUN_TEST_CMD) step2_eval.mal ../../stepA_mal
	$(RUN_TEST_CMD) step3_env.mal ../../stepA_mal
	$(RUN_TEST_CMD) step4_if_fn_do.mal ../../stepA_mal
	$(RUN_TEST_CMD) step5_tco.mal ../../stepA_mal
	$(RUN_TEST_CMD) step6_file.mal ../../stepA_mal
	$(RUN_TEST_CMD) step7_quote.mal ../../stepA_mal
	$(RUN_TEST_CMD) step8_macros.mal ../../stepA_mal
	$(RUN_TEST_CMD) step9_try.mal ../../stepA_mal
	$(RUN_TEST_CMD) stepA_mal.mal ../../stepA_mal

test-malcc: all
	$(RUN_TEST_CMD) step2_eval.mal ../../malcc
	$(RUN_TEST_CMD) step3_env.mal ../../malcc
	$(RUN_TEST_CMD) step4_if_fn_do.mal ../../malcc
	$(RUN_TEST_CMD) step5_tco.mal ../../malcc
	$(RUN_TEST_CMD) step6_file.mal ../../malcc
	$(RUN_TEST_CMD) step7_quote.mal ../../malcc
	$(RUN_TEST_CMD) step8_macros.mal ../../malcc
	$(RUN_TEST_CMD) step9_try.mal ../../malcc
	$(RUN_TEST_CMD) stepA_mal.mal ../../malcc

test-self-hosted: all
	$(RUN_TEST_CMD) --test-timeout 30 step2_eval.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step3_env.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step4_if_fn_do.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step5_tco.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step6_file.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step7_quote.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step8_macros.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 step9_try.mal ../../self_hosted_run
	$(RUN_TEST_CMD) --test-timeout 30 stepA_mal.mal ../../self_hosted_run

test-supplemental: all
	$(RUN_TEST_CMD) --test-timeout 30 ../../tests/utf-8.mal ../../malcc

test-mal-in-mal: mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step2_eval.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step3_env.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step4_if_fn_do.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step5_tco.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step6_file.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step7_quote.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step8_macros.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 step9_try.mal ../../mal-in-mal
	$(RUN_TEST_CMD) --test-timeout 30 stepA_mal.mal ../../mal-in-mal

perf: all
	cd mal/tests && ../../malcc perf1.mal && ../../malcc perf2.mal && ../../malcc perf3.mal

cloc:
	cloc --exclude-dir='tinycc,mal' --not-match-f='hashmap.*|step.*' .

docker-build:
	docker build . -t malcc

RUN_DOCKER_CMD=docker run --security-opt seccomp=unconfined -t -i --rm -v $(PWD):/malcc malcc

docker-bash: docker-build
	$(RUN_DOCKER_CMD) bash

docker-test: docker-build
	$(RUN_DOCKER_CMD) make test

docker-test-supplemental: docker-build
	$(RUN_DOCKER_CMD) make test-supplemental

docker-watch: docker-build
	$(RUN_DOCKER_CMD) bash -c "ls *.c *.h Makefile | entr -c -s 'make test'"

update-mal-directory:
	rm -rf /tmp/mal mal
	mkdir mal
	git clone https://github.com/kanaka/mal.git /tmp/mal
	cp -r /tmp/mal/LICENSE /tmp/mal/Makefile /tmp/mal/README.md /tmp/mal/core.mal /tmp/mal/mal /tmp/mal/perf.mal /tmp/mal/run_argv_test.sh /tmp/mal/runtest.py /tmp/mal/tests mal/
