.PHONY: all clean distclean setup build doc install test-build test 
all: build 
# build

J ?= 2
NAME=pttcp

-include Makefile.config

unix:
	cd lib && $(MAKE)

xen:
	cd lib && $(MAKE) xen

clean: # setup.data setup.bin
	./setup.bin -clean $(OFLAGS)
	cd lib && $(MAKE) clean

run:
	sudo ./lib/_build/main.native

distclean: setup.data setup.bin
	./setup.bin -distclean $(OFLAGS)
	$(RM) setup.bin

setup: setup.data

build: setup.data  setup.bin
	./setup.bin -build -j $(J) $(OFLAGS)

doc: setup.data setup.bin
	./setup.bin -doc -j $(J) $(OFLAGS)

install: 
	ocamlfind remove $(NAME) $(OFLAGS)
	./setup.bin -install

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	$(RM) setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure --enable-tests
