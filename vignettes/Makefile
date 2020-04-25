all: build clean

article.tex: article.Rnw
	"$(R_HOME)/bin/R" CMD Sweave article.Rnw

article.R: article.Rnw
	"$(R_HOME)/bin/R" CMD Stangle article.Rnw

build: article.tex refs.bib
	pdflatex article.tex
	bibtex article.aux
	pdflatex article.tex
	pdflatex article.tex

clean:
	rm -f *.aux *.bbl *.blg *.fls *.log *.out *.fdb_latexmk *.tex *.R img/fig-*
