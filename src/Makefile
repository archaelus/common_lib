# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../common.mk

EBIN = ../ebin
INCLUDE = ../include
DOCDIR = ../doc

# preprocess and includes are necessary to allow semicolons in macros.
EDOC_OPTS = [{title,"Common Library"}, \
	preprocess, {includes, ["$(INCLUDE)", "$(XMERL_APP)/include"]}]

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
EDOC_MODULES = \
	binary \
	my_calendar \
	my_lists \
	my_string \
	stats \
	xml

MODULES = $(EDOC_MODULES)

EXTERNAL_HEADERS = 

INTERNAL_HEADERS = 

ERL_FILES= $(MODULES:%=%.erl)
HRL_FILES = 

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

doc:	
	$(ERL) -noshell $(EDOC_PATHS) -run edoc_run application \
		"'$(APPNAME)'" '".."' '$(EDOC_OPTS)' -s erlang halt

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
$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) $(REQUIRED_FLAGS) \
	$(REQUIRED_PATHS) -o $(EBIN) $<


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPNAME)-$(VSN)

release_src:
	$(INSTALL_DATA) $(MODULES:%=%.erl) $(RELSYSDIR)/src
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

# ----------------------------------------------------
# .hrl dependencies
# ----------------------------------------------------
$(EBIN)/xml.beam:$(XMERL_APP)/include/xmerl.hrl
