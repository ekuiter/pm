.DEFAULT_GOAL := all

CCL_EVAL = "(progn (load \"~/quicklisp/setup.lisp\") (load \"packages.lisp\") (setf ccl:*break-hook* (lambda (cond hook) (declare (ignore cond hook)) (ccl:quit))))"

all:
	rm -f *.dx64fsl
	echo "(ccl:save-application \"pm\" :prepend-kernel t  :toplevel-function #'gui:gui)" | ccl64 -e $(CCL_EVAL)
	cp pm pm.app/Contents/MacOS/
	cp -R res pm.app/Contents/MacOS/

run: all
	./pm

clean:
	rm -f pm

.PHONY: all run clean
