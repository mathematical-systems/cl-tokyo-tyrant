MAKEINFO  = makeinfo
TEXI2HTML = texi2html
TEXI2DVI = texi2dvi
TEX = ptex
DVIPDF = dvipdfmx

all: html

info:
	$(MAKEINFO) index.texi

html:
	$(TEXI2HTML) --init-file ja-init.pl index.texi

dvi:
	TEX=$(TEX) $(TEXI2DVI) --texinfo=@setcontentsaftertitlepage --texinfo=@afourpaper index.texi

pdf: dvi
	$(DVIPDF) index.dvi

.PHONY: clean
clean:
	rm -f index.info* index.html
