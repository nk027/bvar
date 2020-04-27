all: pdf clean

tex: article.tex
code: article.R
pdf: article.pdf

article.tex: article.Rnw
	"$(R_HOME)/bin/R" CMD Sweave article.Rnw

article.R: article.Rnw
	"$(R_HOME)/bin/R" CMD Stangle article.Rnw

article.pdf: article.tex refs.bib
	pdflatex article.tex
	bibtex article.aux
	pdflatex article.tex
	pdflatex article.tex

jss: article_jss.Rnw refs.bib
	"$(R_HOME)/bin/R" CMD Sweave article_jss.Rnw
	"$(R_HOME)/bin/R" CMD Stangle article_jss.Rnw
	pdflatex article_jss.tex
	bibtex article_jss.aux
	pdflatex article_jss.tex
	pdflatex article_jss.tex

clean:
	rm -f *.aux *.bbl *.blg *.fls *.log *.out *.fdb_latexmk *.tex *.R fig-*
