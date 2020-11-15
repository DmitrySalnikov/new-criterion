ifndef name
name = salnikov
endif

all:
	pdflatex $(name).tex
	evince $(name).pdf
	rm *.aux *.log *.out
clean:
	rm *.aux *.log *.out