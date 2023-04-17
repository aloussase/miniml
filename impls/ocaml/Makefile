OCAML := ocamlopt

SRCS := lexer.ml main.ml
OBJS := $(SRCS:%.ml=build/%.cmx)

build/%.cmx: %.ml
	mkdir -p build
	$(OCAML) -I build -c $< -o $@

miniml: $(OBJS)
	$(OCAML) -I build -o $@ $^

clean:
	$(RM) -r build miniml *.cmx *.cmi *.o

.PHONY: clean
