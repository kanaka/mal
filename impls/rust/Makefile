
EXEC_DIR := target/release

UPPER_STEPS := $(EXEC_DIR)/step4_if_fn_do \
               $(EXEC_DIR)/step5_tco \
               $(EXEC_DIR)/step6_file \
               $(EXEC_DIR)/step7_quote \
               $(EXEC_DIR)/step8_macros \
               $(EXEC_DIR)/step9_try \
               $(EXEC_DIR)/stepA_mal
STEP0       := $(EXEC_DIR)/step0_repl
STEP1-2     := $(EXEC_DIR)/step1_read_print \
               $(EXEC_DIR)/step2_eval
STEP3       := $(EXEC_DIR)/step3_env
STEPS := $(STEP0) $(STEP1-2) $(STEP3) $(UPPER_STEPS)

all: $(STEPS)

$(STEPS): $(EXEC_DIR)/%: %.rs
	cargo build --release --bin $*

$(STEP1-2) $(STEP3) $(UPPER_STEPS): types.rs reader.rs printer.rs
$(STEP3) $(UPPER_STEPS): env.rs
$(UPPER_STEPS): core.rs

lint:
	rustfmt *.rs
	cargo clippy

.PHONY: clean

clean:
	cargo clean
