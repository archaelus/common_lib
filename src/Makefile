# FIXME These variables should be automatically set when compiling
ERL = erl
ERLC = erlc
ETAGS = etags
EMULATOR = beam
EBIN = ../ebin
INCLUDE = ../include
ESRC = ./
DOCDIR = ../doc
RM = rm -f
INSTALL = /usr/bin/install -c
INSTALL_DIR = /usr/bin/install -c -d
INSTALL_DATA = ${INSTALL} -m 644


### common_lib use eDoc for documentation, to regenerate update paths as
### needed!
COMMON_LIB_APP = ..
EDOC_APP = /usr/local/lib/erlang/lib/edoc-0.4
SYNTAX_TOOLS_APP = /usr/local/lib/erlang/lib/syntax_tools-1.3



# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COMMON_LIB_VSN)
APPNAME = common_lib

RELSYSDIR = $(RELEASE_PATH)/lib/$(APPNAME)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EDOC_MODULES = \
	binary \
	my_calendar \
	my_lists \
	my_string \
	stats


MODULES = $(EDOC_MODULES)

HRL_FILES = 

INTERNAL_HRL_FILES  = 

ERL_FILES= $(MODULES:%=%.erl)

DOC_TARGET_FILES = $(EDOC_MODULES:%=$(DOCDIR)/%.html)
TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I $(INCLUDE)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

doc:	$(DOC_TARGET_FILES)

opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES)

realclean: clean
	$(RM) $(DOC_TARGET_FILES)
	$(RM) $(DOCDIR)/edoc-info
	$(RM) $(DOCDIR)/index.html
	$(RM) $(DOCDIR)/modules-frame.html
	$(RM) $(DOCDIR)/overview-summary.html
	$(RM) $(DOCDIR)/packages-frame.html
	$(RM) $(DOCDIR)/stylesheet.css

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(COMMON_LIB_APP)/ebin \
	-pa $(SYNTAX_TOOLS_APP)/ebin


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<


$(DOCDIR)/%.html:	%.erl
	$(ERL) $(EDOC_PATHS) -s edoc file $< ['{dir,"$(DOCDIR)"}'] -s erlang halt


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
release_src:
	$(INSTALL_DATA) $(MODULES:%=%.erl) $(RELSYSDIR)/src
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
