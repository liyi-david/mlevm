TARGET_DIR	?= $(CURDIR)

OCAMLFIND	?= ocamlfind
OCAMLOPT	?= ocamlopt
OCAMLC		?= ocamlc

INCLUDES	=  $(CURDIR)/src $(CURDIR)/gen

INTERFACES	=  $(wildcard $(CURDIR)/src/*.mli)
SOURCES		=  $(CURDIR)/gen/evm.ml $(wildcard $(CURDIR)/src/*.ml)

CMIS		=  $(INTERFACES:.mli=.cmi)
# make sure there is no space befor/after the comma, very important!
PACKAGES	=  num,ctypes

default: $(TARGET_DIR)/libcoqvm.a

$(TARGET_DIR)/libcoqvm.o: $(CMIS) $(SOURCES)
	$(OCAMLFIND) \
		$(OCAMLOPT) -output-complete-obj \
		-o $@ \
		$(addprefix -I , $(INCLUDES)) \
		-linkpkg -package $(PACKAGES) \
		$(SOURCES)

$(CURDIR)/gen/evm.ml: $(CURDIR)/gen/evm.v
	cd $(CURDIR)/gen && coqc $(CURDIR)/gen/evm.v
	rm $(CURDIR)/gen/evm.mli

%.a: %.o
	ar qs $@ $<
	mv $< $<.backup

clean:
	rm -f $(CURDIR)/*.a
	rm -f $(CURDIR)/*.o
	rm -f $(CURDIR)/src/*.o
	rm -f $(CURDIR)/src/*.mli
	rm -f $(CURDIR)/src/*.cmi
	rm -f $(CURDIR)/src/*.cmx
	rm -f $(CURDIR)/gen/*.ml
	rm -f $(CURDIR)/gen/*.mli
	rm -f $(CURDIR)/gen/*.cmi
	rm -f $(CURDIR)/gen/*.cmx
	rm -f $(CURDIR)/gen/*.glob
	rm -f $(CURDIR)/gen/*.o
	rm -f $(CURDIR)/gen/*.vo
	rm -f $(CMIS)
