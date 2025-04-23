.POSIX:
.SUFFIXES: .el .elc

EMACS = emacs
RM = rm -f

compile: renpy.elc

.el.elc:
	$(EMACS) --batch --quick \
	    --directory . \
	    --funcall batch-byte-compile $<

clean:
	$(RM) renpy.elc
