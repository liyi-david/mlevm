TARGET_DIR	?= $(CURDIR)

OCAMLFIND	?= ocamlfind
OCAMLOPT	?= ocamlopt

SOURCE_DIRS	=  $(CURDIR)/src,$(CURDIR)/gen
SOURCES		=  $(wildcard $(CURDIR)/src/*.ml $(CURDIR)/gen/*.ml)
# make sure there is no space befor/after the comma, very important!
PACKAGES	=  num,ctypes

default: $(TARGET_DIR)/libevm.a

$(TARGET_DIR)/libevm.o:
	ocamlfind \
		ocamlopt -output-complete-obj \
		-cc "gcc" \
		-o $@ \
		-I $(SOURCE_DIRS) \
		-linkpkg -package $(PACKAGES) \
		$(SOURCES)

%.a: %.o
	ar qs $@ $<

clean:
	rm -f $(CURDIR)/*.a
	rm -f $(CURDIR)/*.o
