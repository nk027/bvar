all: sweave build clean

sweave:
	"$(R_HOME)/bin/R" CMD Sweave article.Rnw

stangle:
	"$(R_HOME)/bin/R" CMD Stangle article.Rnw

build: article.Rnw
	pdflatex article.tex
	bibtex article.aux
	pdflatex article.tex
	pdflatex article.tex

clean:
	rm -f *.aux *.bbl *.blg *.fls *.log *.out *.fdb_latexmk fig-*
