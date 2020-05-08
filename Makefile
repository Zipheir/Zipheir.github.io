include pages.mk

HTML = $(PAGES:.md=.html)
HEADER = head.html
FOOTER = foot.html
INCS = $(HEADER) $(FOOTER)

all: $(HTML) $(INCS)

.SUFFIXES: .md .html

.md.html:
	@echo generating $@
	@./title.sed < $< | markdown | cat $(HEADER) - $(FOOTER) > $@

clean:
	rm -f $(HTML)
