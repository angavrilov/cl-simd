

all: cl-simd.html cl-simd.pdf
%.html: %.md
	pandoc $< -o $@
%.pdf: %.md
	pandoc $< -o $@
