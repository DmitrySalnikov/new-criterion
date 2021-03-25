ifndef name
name = salnikov
endif

all:
	pdflatex $(name).tex
	pdflatex $(name).tex
	evince $(name).pdf
	rm *.aux *.log *.out *.spl *.bbl *.blg
clean:
	rm *.aux *.log *.out *.spl *.bbl *.blg