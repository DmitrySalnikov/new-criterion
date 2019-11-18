all:
	pdflatex salnikov.tex
	evince salnikov.pdf
	rm *.aux *.log *.out
clean:
	rm *.aux *.log *.out