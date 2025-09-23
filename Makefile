.POSIX:
.SUFFIXES: .el .elc

EMACS = emacs
RM = rm -f

compile: renpy-mode.elc

.el.elc:
	$(EMACS) --batch --quick \
	    --directory . \
	    --funcall batch-byte-compile $<

clean:
	$(RM) renpy-mode.elc
