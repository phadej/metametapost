PROJECT=mmp

all : $(PROJECT).pdf $(PROJECT)-with-notes.pdf

%.pdf: %.tex $(PROJECT)core.tex
	pdflatex $^

$(PROJECT)core.tex: $(PROJECT).lhs
	lhs2TeX -o $(PROJECT)core.tex $(PROJECT).lhs
