PANDOCOPTIONS=--listings --number-sections -V graphics --biblatex --template=latex/template.latex

all: paper.pdf

%.pdf: %.tex %.bib
	latexmk --quiet -pdf $*

paper.tex: paper.md Makefile
	@echo Rebuilding paper.tex since [$?] changed
	pandoc -s ${PANDOCOPTIONS} -t latex -o paper.tex paper.md

clean:
	-latexmk -C paper
	-rm paper.bbl
	-rm paper.run.xml
	-rm paper.tex

