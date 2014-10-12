SOURCES = midi.ml musical_transformations.ml open_window.ml #export_and_listen.ml
OBJECTS = $(SOURCES:%.ml=%.cmo)
FINAL = $(SOURCES:%.ml=%.cmi)
GRAPH_FLAGS = graphics.cma
all: projet

projet: $(OBJECTS)
	ocamlc -o $@ $(GRAPH_FLAGS) $^

%.cmo: %.ml
	ocamlc -c $<

clean:
	$(RM) $(OBJECTS) $(FINAL)
	$(RM) projet
