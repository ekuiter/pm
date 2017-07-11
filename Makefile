.DEFAULT_GOAL := all

all:
	rm -f *.dx64fsl
	ccl -e "(progn (load \"~/quicklisp/setup.lisp\") (load \"pm.lisp\") (ccl:save-application \"pm\" :toplevel-function 'gui :prepend-kernel t))"
	cp pm pm.app/Contents/MacOS/
	cp -R res pm.app/Contents/MacOS/

run: all
	./pm

clean:
	rm -f pm

.PHONY: all run clean
