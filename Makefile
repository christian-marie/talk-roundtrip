MARKDOWNS=slides.md handout.md rd.md
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)
clean:
	rm -f $(PDFS)

slides.pdf: slides.md template.tex
	pandoc -H template.tex --filter columnfilter.py -t beamer --highlight-style=haddock -s $< -o $@

rd.pdf: rd.md template.tex
	pandoc -H template.tex --filter columnfilter.py -i -t beamer --highlight-style=haddock -s $< -o $@

handout.pdf: handout.md
	pandoc --template=handout.tex -V geometry:margin=1in -V papersize:"a4paper" --highlight-style=haddock --reference-links $< -o $@
