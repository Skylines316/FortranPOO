set xlabel "r"
set xrange [0:1]
set yrange [0:1.2e11]
set ylabel "U"
set term png
set output "./figures/E=-0_025.png"
m = "./data/datos.dat"
set nokey
set grid
set title "Potencial"
plot m using 1:2 with lines
set term x11