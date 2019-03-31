SOURCES := $(shell fd . './lisp' -e "el")
OBJECTS := $(SOURCES:%.el=%.elc)

.PHONY: all clean

all: $(OBJECTS) init.elc

clean:
	fd -e "elc" -x rm

%.elc: %.el
	emacs -Q --batch \
    --eval '(setq use-package-expand-minimally t)' \
    -f batch-byte-compile $<
