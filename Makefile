HTML=$(addsuffix .html,$(basename $(shell find . -name '*.markdown')))

%.html: %.markdown
	maruku $< -o $@

html: $(HTML)
