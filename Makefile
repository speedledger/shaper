PREFIX?=${HOME}/.local
SHARED=${PREFIX}/share/shaper
BIN=${PREFIX}/bin
EXECUTABLE?=shaper

.PHONY: install uninstall

install:
ifeq "${shell readlink ${BIN}/${EXECUTABLE}}" ""
	mkdir -p ${BIN}
	mkdir -p ${SHARED}/
	mkdir -p ${SHARED}/src
	cp shaper ${SHARED}/shaper
	cp shaper.sc ${SHARED}/shaper.sc
	cp README.md ${SHARED}/README.md
	cp src/*.sc ${SHARED}/src/
	ln -s ${SHARED}/shaper ${BIN}/${EXECUTABLE}
else
	@echo "${BIN}/${EXECUTABLE} already exists. Maybe uninstall first with \`make uninstall\`"
	@exit 1
endif

uninstall:
ifneq "${PREFIX}" ""
ifeq "$(shell readlink ${BIN}/${EXECUTABLE})" "$(shell [ -f ${SHARED}/shaper ] && ls ${SHARED}/shaper)"
ifneq "$(shell readlink ${BIN}/${EXECUTABLE})" ""
	rm ${BIN}/${EXECUTABLE}
	rm ${SHARED}/shaper
	rm ${SHARED}/shaper.sc
	rm ${SHARED}/README.md
	rm ${SHARED}/src/*.sc
	rmdir ${SHARED}/src
	rmdir ${SHARED}
else
	@echo "${BIN}/${EXECUTABLE} is not installed."
	@exit 1
endif
else
	@echo "Sanity check failed, ${BIN}/${EXECUTABLE} is not correctly linked for uninstall"
	@exit 1
endif
else
	@echo "Incorrectly set PREFIX"
	@exit 1
endif