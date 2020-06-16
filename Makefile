PREFIX?=${HOME}/.local
SHARED=${PREFIX}/share/shaper
BIN=${PREFIX}/bin
EXECUTABLE?=shaper

install:
	mkdir -p ${BIN}
	mkdir -p ${SHARED}/
	mkdir -p ${SHARED}/src
	cp shaper ${SHARED}/shaper
	cp shaper.sc ${SHARED}/shaper.sc
	cp README.md ${SHARED}/README.md
	cp src/*.sc ${SHARED}/src/
	ln -s ${SHARED}/shaper ${BIN}/${EXECUTABLE}