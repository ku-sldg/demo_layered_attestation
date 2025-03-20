TEX_SRCS := $(wildcard *.tex)

%.pdf:	%.tex
	pdflatex $*
	pdflatex $*

%.ps:	%.tex
	latex $*
	latex $*
	dvips -o $@ $*

all:	$(TEX_SRCS:.tex=.pdf)

clean:
	-rm $(TEX_SRCS:.tex=.pdf) $(TEX_SRCS:.tex=.log) $(TEX_SRCS:.tex=.aux)
