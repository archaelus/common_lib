# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include common.mk

SUB_DIRECTORIES = src

OTHER_FILES = COPYING Makefile changes.txt common.mk $(APPNAME).pub 

BASE_REL = /var/tmp/$(APPNAME)-$(VSN)

DOC_OPTS = [{title,"Common Library"}]

SPECIAL_TARGETS = 

all:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE)); \
	done

docs:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; $(MAKE) doc); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) clean); \
	done

realclean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) realclean); \
	done

debug:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) DEBUG=-Ddebug=1); \
	done

release:
	mkdir $(BASE_REL)
	cp -p $(OTHER_FILES) $(BASE_REL)
	mkdir $(BASE_REL)/ebin
	cp -p ebin/*.app $(BASE_REL)/ebin
	mkdir $(BASE_REL)/doc
	cp -p doc/*.html $(BASE_REL)/doc
	cp -p doc/stylesheet.css $(BASE_REL)/doc
	cp -p doc/overview.edoc $(BASE_REL)/doc
	mkdir $(BASE_REL)/priv
	mkdir $(BASE_REL)/include
	mkdir $(BASE_REL)/src
	cd src; make RELSYSDIR=$(BASE_REL) release_src
	cd $(BASE_REL)/..; tar -czvf $(APPNAME)-$(VSN).tar.gz $(APPNAME)-$(VSN)
	mv $(BASE_REL)/../$(APPNAME)-$(VSN).tar.gz .
	rm -rf $(BASE_REL)


