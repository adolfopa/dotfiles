check: check-emacs check-zsh

check-emacs:
	diff ~/.emacs emacs/.emacs

check-zsh:
	diff ~/.zshrc zsh/.zshrc ; \
            diff ~/.oh-my-zsh/custom/themes/xardon.zsh-theme zsh/xardon.zsh-theme

install: install-emacs install-zsh

install-emacs:
	cp emacs/.emacs ~/.emacs

install-zsh:
	cp zsh/.zshrc ~/ && \
            mkdir -p ~/.oh-my-zsh/custom/themes && \
            cp zsh/xardon.zsh-theme ~/.oh-my-zsh/custom/themes

install-scripts:
	cp scripts/* ~/bin
