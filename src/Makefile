.PHONY: 	all clean byte native debug sanity
# USE: make all, make byte etc.

# can view actual dependencies as dotfiles: ie for parser.mly check ._parser.mly

# Dependencies: menhir, sexplib, ppx_sexp_conv
# do: eval $(opam config env) every new terminal

# may need to opam install str

OCB_FLAGS   = -use-ocamlfind -package str -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)


all: native

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

# not a portable executable.
# profile: sanity
# 	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

# If ocamlbuild command is not found: run: eval $(opam config env)
sanity:
	which menhir

