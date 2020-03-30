#!/usr/bin/sh

if [ -d "./extensions/yasnippet" ]; then
    echo "Initializing Submodules"
    git submodule update --init --recursive --depth 1
else
    echo "Update Submodules"
    git submodule foreach "git pull origin master"
fi

$(cd extensions/org-mode ; make autoloads ; make ; sudo make install)
$(cd extensions/auctex ; ./configure ; make; sudo make install)
$(make -C extensions/liberime)
emacs --batch --eval "(byte-recompile-directory (expand-file-name \"./extensions\") 0)"

# do not generate *.elc file for snails
rm extensions/snails/*.elc
