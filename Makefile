mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))

PLASMAPKG ?= $(which plasmapkg2)

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

MYEMACS := $(realpath $(dir $(mkfile_path))/emacs.el)
EMACSINIT := $(firstword $(wildcard $(HOME)/.emacs $(HOME)/.emacs.el $(HOME)/.emacs.d/init.el) $(HOME)/.emacs)

install-emacs:
ifeq ($(shell grep -qF '(load-file "$(MYEMACS)")' $(EMACSINIT) 2>/dev/null && echo FOUND), FOUND)
	@echo "$(EMACSINIT) was already modified"
else
	(echo '' \
	&& echo ';; Added by $(mkfile_path)' \
	&& echo '(if (file-exists-p "$(MYEMACS)")' \
	&& echo '    (load-file "$(MYEMACS)")' \
	&& echo "  (warn \"Can't find '$(MYEMACS)', not loaded!\")" \
	&& echo '  )') >> $(EMACSINIT)
endif
.PHONY: install-emacs
install: install-emacs

clean-emacs :
	@echo '***'
	@echo '*** To clean $(EMACSINIT), look for the comment "Added by $(mkfile_path)"'
	@echo '***'
.PHONY: clean-emacs
clean: clean-emacs

###########################################################################

$(HOME)/.local/share/ktexteditor_snippets:
	mkdir -p $(dir $@)
	ln -s "$(dir $(mkfile_path))/ktexteditor_snippets" $@

install-kate-snippets: $(HOME)/.local/share/ktexteditor_snippets
.PHONY: install-kate-snippets
install: install-kate-snippets

###########################################################################

ifneq "$(PLASMAPKG)" ""
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
endif

