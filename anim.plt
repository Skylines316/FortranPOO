# set xlabel "theta"
set xrange [-3:3]
set yrange [-3:3]
# set ylabel "r"
set term png
set nokey
set title "Orbita"
set polar
# plot for[i = 1:100] "./temp/data".i.".dat" using 1:2 with lines
set output "./temp/frame".num.".png"
plot "./temp/data".num.".dat" using 1:2 with lines
set term x11