include pages.mk

HTML = $(PAGES:.md=.html)
HEADER = head.html
FOOTER = foot.html
INCS = $(HEADER) $(FOOTER)

all: $(HEADER) $(FOOTER) $(HTML) $(INCS)

.SUFFIXES: .md .html

.md.html:
	@echo generating $@
	@./title.sh < $< | markdown | cat $(HEADER) - $(FOOTER) > $@

clean:
	rm -f $(HTML)
