OCAML := ocamlopt

SRCS := lexer.ml syntax.ml parser.ml main.ml
OBJS := $(SRCS:%.ml=build/%.cmx)

build/%.cmx: %.ml
	mkdir -p build
	$(OCAML) -I build -c $< -o $@

miniml: $(OBJS)
	$(OCAML) -o $@ $^

clean:
	$(RM) -r build miniml *.cmx *.cmi *.o

.PHONY: clean
