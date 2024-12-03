compile := main.ml
target := run
input := test_input

run: build
	./$(target) < $(input)

build:
	ocamlc -I +str str.cma -o $(target) $(compile)

clean:
	rm *.cm*
