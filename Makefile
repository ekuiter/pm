.DEFAULT_GOAL := all

all:
	ccl -e "(progn (load \"~/quicklisp/setup.lisp\") (load \"pm.lisp\") (ccl:save-application \"pm\" :toplevel-function 'gui :prepend-kernel t))"
	cp pm pm.app/Contents/MacOS/

run: all
	./pm
	
clean:
	rm pm
	
.PHONY: all run clean
