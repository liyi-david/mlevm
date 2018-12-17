OCAML_PACKAGES		= num
OCAML_SOURCE_DIRS	= src/interface
OCAML_FILES			= src/interface/interpreter.ml

OCAMLFIND    		= ocamlfind
OCAMLOPT			= ocamlopt

RUSTC				= rustc

TARGET_EVMLIB		= target/libevm.o
TARGET_EVMLIB_A		= target/libevm.a
TARGET_LEVM			= target/levm

$(TARGET_EVMLIB): $(OCAML_FILES)
	$(OCAMLFIND) $(OCAMLOPT) -output-complete-obj -o $@ \
		-linkpkg -package $(OCAML_PACKAGES) \
		$(OCAML_FILES)

$(TARGET_EVMLIB_A): $(TARGET_EVMLIB)
	ar qs $(TARGET_EVMLIB_A) $(TARGET_EVMLIB)

$(TARGET_LEVM): src/rust_interface/levm.rs $(TARGET_EVMLIB_A)
	$(RUSTC) $< -L ./target -o $@

all: $(TARGET_LEVM)

run: all
	./target/levm

clean:
	rm -f $(OCAML_SOURCE_DIRS)/*.o
	rm -f $(OCAML_SOURCE_DIRS)/*.cmo
	rm -f $(OCAML_SOURCE_DIRS)/*.cma
	rm -f $(OCAML_SOURCE_DIRS)/*.cmx
	rm -f $(OCAML_SOURCE_DIRS)/*.cmxa
	rm -rf target/*
