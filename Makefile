DEST=msx-forth83
GIT=git
COMMIT=d3d42fc743ab30e3775b6b6a95431a24fd3d1b22

.PHONY: all test dsk clean create update
all: update
	cp compile.tcl msx-forth83/
	cp dsk/* msx-forth83/dsk
	cp src/*.4th ${DEST}/src
	make -C ${DEST} all

test: all
	cp tests/*.4th ${DEST}/tests
	make -C ${DEST} test

dsk: all
	make -C ${DEST} dsk

clean:
	rm -r msx-forth83

update: create
	cd msx-forth83
	${GIT} submodule update

create: msx-forth83 files

msx-forth83:
	mkdir -p msx-forth83
	${GIT} submodule init
	${GIT} submodule update

files:
	cp dsk/* msx-forth83/dsk

