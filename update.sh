#!/usr/bin/bash

# update git submodule
if [ -d "./extensions/yasnippet" ]; then
    echo "Initializing Submodules"
    git submodule update --init --recursive --depth 1
else
    echo "Update Submodules"
    git submodule foreach "git pull origin master"
fi

root=$(pwd)

# build org-mode
cd $root/extensions/org-mode
make autoloads
make all
sudo make install

# build auctex
cd $root/extensions/auctex
./configure
make
sudo make install

# build liberime
make -C $root/extensions/liberime

# build magit
cd $root/extensions/libgit
git submodule init
git submodule update
mkdir build
cd build
cmake ..
make

make -C $root/extensions/magit

# compile .elc
emacs --batch -l $root/init.el --eval "(byte-recompile-directory (expand-file-name \"./extensions\") 0)"

# do not generate *.elc file for snails
rm extensions/snails/*.elc || true
