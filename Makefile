check: check-emacs check-zsh

check-emacs:
	diff -u ~/.emacs emacs/.emacs

check-zsh:
	diff -u ~/.zshrc zsh/.zshrc ; \
            diff ~/.oh-my-zsh/custom/themes/xardon.zsh-theme zsh/xardon.zsh-theme

install: install-binaries install-emacs install-zsh install-scripts

install-binaries: mk-bin-dir build
	cp src/affix ~/bin

install-emacs:
	cp emacs/.emacs ~/.emacs

install-zsh:
	cp zsh/.zshrc ~/ && \
            mkdir -p ~/.oh-my-zsh/custom/themes && \
            cp zsh/xardon.zsh-theme ~/.oh-my-zsh/custom/themes

install-scripts: mk-bin-dir
	cp scripts/* ~/bin

build:
	cd src && $(MAKE)

mk-bin-dir:
	[ ! -d ~/bin ] && mkdir ~/bin || true

clean:
	cd src && $(MAKE) clean && find . -type f -name '*~' | xargs rm
