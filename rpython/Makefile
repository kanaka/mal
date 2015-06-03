
RPYTHON = rpython
#PYTHONPATH = /home/joelm/scratch/pypy/rpython
#export PYTHONPATH

STEPS: step0_repl step1_read_print

all: $(STEPS)

%: %.py
	$(RPYTHON) --output=$@ $<

step1_read_print: mal_types.py reader.py printer.py

clean:
	rm -f $(STEPS) *.pyc
