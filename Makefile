WARN = -w A-32-67-70

#COMP = byte
COMP = native

ifeq (${COMP},native)
OCAMLC = ocamlfind ocamlopt ${WARN}
EXT    = cmx
else
OCAMLC = ocamlfind ocamlc -g ${WARN}
EXT    = cmo
endif

SRCS  = pqueue.ml boardrep.ml boardmetric.ml board.ml astar.ml npuzzle.ml
OBJS  = ${patsubst %.ml, %.${EXT}, ${SRCS}}
PKGS  = batteries,ounit2
PROG  = npuzzle

compile:
	${OCAMLC} -c pqueue.mli
	${OCAMLC} -c pqueue.ml -package ${PKGS}
	${OCAMLC} -c boardrep.mli
	${OCAMLC} -c boardrep.ml
	${OCAMLC} -c boardmetric.mli
	${OCAMLC} -c boardmetric.ml
	${OCAMLC} -c board.mli
	${OCAMLC} -c board.ml
	${OCAMLC} -c astar.mli
	${OCAMLC} -c astar.ml
	${OCAMLC} -c npuzzle.mli
	${OCAMLC} -c npuzzle.ml
	${OCAMLC} -c main.ml
	${OCAMLC} -package ${PKGS} -linkpkg ${OBJS} main.${EXT} -o ${PROG}

test_partA: tests_final_partA.ml ${OBJS}
	${OCAMLC} -o tests_final_partA -package ${PKGS} -linkpkg ${OBJS} tests_final_partA.ml
	./tests_final_partA

test_partB: tests_final_partB.ml ${OBJS}
	${OCAMLC} -o tests_final_partB -package ${PKGS} -linkpkg ${OBJS} tests_final_partB.ml
	./tests_final_partB

test_partC: tests_final_partC.ml ${OBJS}
	${OCAMLC} -o tests_final_partC -package ${PKGS} -linkpkg ${OBJS} tests_final_partC.ml
	./tests_final_partC

test: test_partA test_partB test_partC

all: compile test

clean:
	rm -f *.cmi *.cmo *.cmx *.o ${PROG}
	rm -f npuzzle.out
	rm -f tests_final_partA tests_final_partB tests_final_partC
	rm -f *.log *.cache

