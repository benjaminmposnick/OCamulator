MODULES=linalg prob solve ast main parser lexer vector matrix eval
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

test: clean
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
	bisect-ppx-report html --ignore-missing-files

start:
	$(OCAMLBUILD) $(MAIN) && ledit ./$(MAIN)

start_no_keys:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean
	rm -rf _coverage bisect*.coverage

zip:
	zip ocamulator.zip *.ml* *.md Makefile _tags .ocamlinit .merlin .gitignore