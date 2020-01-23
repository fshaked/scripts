mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))

PLASMAPKG ?= plasmapkg2

ZIP ?= zip

all:
.PHONY: all

install:
.PHONY: install

clean:
.PHONY: clean

###########################################################################

MYBASHRC := $(dir $(mkfile_path))/bashrc

install-bashrc:
ifeq ($(shell grep -qF '. "$(MYBASHRC)"' ~/.bashrc 2>/dev/null && echo FOUND), FOUND)
	@echo ".bashrc was already modified"
else
	(echo ''\
	&& echo '# Added by $(mkfile_path)'\
	&& echo 'if [ -f "$(MYBASHRC)" ]; then'\
	&& echo '  . "$(MYBASHRC)"'\
	&& echo 'fi') >> ~/.bashrc
endif
.PHONY: install-bashrc
install: install-bashrc

clean-bashrc:
	@echo '***'
	@echo '*** To clean ~/.bashrc, look for the comment "Added by $(mkfile_path)"'
	@echo '***'
.PHONY: clean-bashrc
clean: clean-bashrc

###########################################################################

toPrimaryScreen.kwinscript:
	$(ZIP) -r $@ toPrimaryScreen
.PHONY: toPrimaryScreen.kwinscript
all: toPrimaryScreen.kwinscript

install-toPrimaryScreen: toPrimaryScreen.kwinscript
	$(PLASMAPKG) -t kwinscript -u $< || $(PLASMAPKG) -t kwinscript -i $<
.PHONY: install-toPrimaryScreen
install: install-toPrimaryScreen

clean-toPrimaryScreen:
	rm -f toPrimaryScreen.kwinscript
	$(PLASMAPKG) -t kwinscript -r toPrimaryScreen.kwinscript
.PHONY: clean-toPrimaryScreen
clean: clean-toPrimaryScreen
