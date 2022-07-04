CC = ocamlc
EXEC = pattern_matching

SOURCES = edge.mli edge.ml graph.mli graph.ml pattern_matching.mli pattern_matching.ml test_pattern_matching.ml

TEMP = $(SOURCES:.mli=.cmi)
OBJS = $(TEMP:.ml=.cmo)

CMO = edge.cmo graph.cmo pattern_matching.cmo test_pattern_matching.cmo

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) -o $@ $(CMO)

%.cmi: %.mli
	$(CC) -o $@ -c $<

%.cmo: %.ml
	$(CC) -o $@ -c $<

clear:
	rm -f *.cmi
	rm -f *.cmo
	rm -f pattern_matching
