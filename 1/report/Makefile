NAME  = main

document: clean
	pdflatex $(NAME).tex
	bibtex $(NAME) || true
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex

clean:
	rm $(NAME).log $(NAME).aux $(NAME).blg $(NAME).out $(NAME).bbl $(NAME).pdf missfont.log $(NAME).lof $(NAME).lot $(NAME).toc || true