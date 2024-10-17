#!/bin/bash
num=$1
if [ -z $num ]; then
  echo Please provide homework number in the format [0-9][0-9]
  echo build.sh NUM [DEADLINE]
  exit
fi
echo $num

deadline=$2
if [ -z $deadline ]; then
  deadline="$(python ./get_deadline.py)"
fi
cat template.tex | sed s/NUMHW/$num/g | sed s/DEADLINE/$deadline/g > hw_tmp.tex
pdflatex -output-directory=./hw$num -jobname=hw$num hw_tmp.tex
rm hw_tmp.tex
ps -o pid -C mupdf | grep -v PID | xargs kill -s HUP
