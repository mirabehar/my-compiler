
PKGS=oUnit,extlib,unix,sexplib
BUILD=ocamlbuild -r -use-ocamlfind -pkg $(PKGS) 

main: main.ml compile.ml runner.ml parser.ml asm.ml
	$(BUILD) main.native
	mv main.native main

test: compile.ml runner.ml test.ml parser.ml asm.ml
	mkdir -p output 
	$(BUILD) test.native
	mv test.native test

output/%.run: output/%.o main.c
	gcc -no-pie -o $@ main.c $<

output/%.o: %.s
	cd output
	gcc -c $<

output/%.s: input/%.331 main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test
