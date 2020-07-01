SRCS = step0_repl.idr step1_read_print.idr step2_eval.idr step3_env.idr \
	   step4_if_fn_do.idr step5_tco.idr step6_file.idr step7_quote.idr \
	   step8_macros.idr
OTHER_SRCS = Types.idr Reader.idr Eval.idr Core.idr MonadTrans.idr
BINS = $(SRCS:%.idr=build/exec/%)

idris_args = -p contrib

#####################

all: $(BINS)

$(BINS): build/exec/%: %.idr $(OTHER_SRCS)
	idris2 $(idris_args) $< -o $(<:%.idr=%)

clean:
	rm -rf build
