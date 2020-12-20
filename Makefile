MODULES=linalg prob solve ast main parser lexer vector matrix eval stat
OBJECTS=$(MODULES:=.cmo)
MLS=linalg.ml prob.ml solve.ml ast.ml main.ml vector.ml matrix.ml eval.ml stat.ml
MLIS=linalg.mli prob.mli solve.mli vector.mli matrix.mli eval.mli stat.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=ounit2,ansiterminal

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
	rm -rf ocamulator.zip doc.public doc.private _coverage* bisect*.coverage __pycache__

zip:
	zip -r ocamulator.zip *.ml* *.md Makefile _tags .ocamlinit .merlin .gitignore

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)