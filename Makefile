MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)
clean:
	rm -f $(PDFS) slides.htm

%.pdf: %.md template.tex
	pandoc -H template.tex --filter columnfilter.py -i -t beamer -s $< -o $@
