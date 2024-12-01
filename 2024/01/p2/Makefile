target := run
input := input

run: build
	./$(target) < $(input)

build:
	ocamlc -I +str str.cma -o $(target) p2.ml

clean:
	rm *.cm*
