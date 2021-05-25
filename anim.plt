set rrange [0:3]
set grid polar
# unset tics
# unset raxis
# c=sprintf("%05d", b)
num=sprintf("%04d", prenum)
set term png
set nokey
set title "Orbita"
set polar
set output "./temp/frame".num.".png"
plot "./temp/data".num.".dat" using 1:2 with lines
set term x11