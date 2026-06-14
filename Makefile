# -- compiler and flags
FC := gfortran

FFLAGS  := -O2 -Wall -ffree-form
PREFIX  := $(HOME)/.local

# -- targets
BIN     := tems
SRC     := main/tems.f

all: $(BIN)

$(BIN): $(SRC)
	$(FC) $(FFLAGS) -o $@ $<

install: $(BIN)
	mkdir -p $(PREFIX)/bin
	cp $(BIN) $(PREFIX)/bin/

uninstall:
	rm -f $(PREFIX)/bin/$(BIN)

clean:
	rm -f $(BIN)

.PHONY: all install uninstall clean
