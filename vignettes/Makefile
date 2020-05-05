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

article_jss.Rnw: article.Rnw
	# Remove nojss, adapt n_, adapt time spent, remove sed_flag lines
	cat article.Rnw | sed -e "s/,nojss//" \
		-e "s/n_draw = [0-9]*/n_draw = 50000/g" \
		-e "s/n_burn = [0-9]*/n_burn = 25000/g" \
		-e "s/n_cores <- [0-9]*/n_cores <- 4/" \
		-e "s/[0-9\.]* \(secs\|mins\)/50.95 secs/g"
		-e "/sed_flag/d" > article_jss.Rnw

jss: article_jss.Rnw refs.bib
	"$(R_HOME)/bin/R" CMD Sweave article_jss.Rnw
	"$(R_HOME)/bin/R" CMD Stangle article_jss.Rnw
	pdflatex article_jss.tex
	bibtex article_jss.aux
	pdflatex article_jss.tex
	pdflatex article_jss.tex

clean:
	rm -f *.aux *.bbl *.blg *.fls *.log *.out *.fdb_latexmk *.tex *.R fig-*
