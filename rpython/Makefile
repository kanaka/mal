
RPYTHON = rpython
#PYTHONPATH = /home/joelm/scratch/pypy/rpython
#export PYTHONPATH

STEPS: step0_repl step1_read_print step2_eval

all: $(STEPS)

%: %.py
	$(RPYTHON) --output=$@ $<

step1_read_print: mal_types.py reader.py printer.py
step2_eval: mal_types.py reader.py printer.py
step3_env: mal_types.py reader.py printer.py env.py
step4_if_fn_do: mal_types.py reader.py printer.py env.py core.py
step5_tco: mal_types.py reader.py printer.py env.py core.py
step6_file: mal_types.py reader.py printer.py env.py core.py
step7_quote: mal_types.py reader.py printer.py env.py core.py
step8_macros: mal_types.py reader.py printer.py env.py core.py
step9_try: mal_types.py reader.py printer.py env.py core.py

clean:
	rm -f $(STEPS) *.pyc
