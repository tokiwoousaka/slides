#!/bin/sh
pname=$1

git clone https://github.com/tokiwoousaka/takahashi_temp.git
mv takahashi_temp $pname
cd $pname
stack build
