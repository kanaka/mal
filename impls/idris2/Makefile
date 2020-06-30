SRCS = step0_repl.idr step1_read_print.idr step2_eval.idr
OTHER_SRCS = Types.idr Reader.idr Eval.idr MonadTrans.idr
BINS = $(SRCS:%.idr=build/exec/%)

idris_args = -p contrib

#####################

all: $(BINS)

$(BINS): build/exec/%: %.idr $(OTHER_SRCS)
	idris2 $(idris_args) $< -o $(<:%.idr=%)

clean:
	rm -rf build
