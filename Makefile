all: index.html outline.html

%.html: %.markdown
	maruku $< -o $@
