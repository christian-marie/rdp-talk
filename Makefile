MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)
clean:
	rm -f $(PDFS)

%.pdf: %.md  template.tex
	pandoc --latex-engine=xelatex -H template.tex -i -t beamer --highlight-style=haddock -s $< -o $@ 
