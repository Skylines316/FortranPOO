set xlabel "theta"
set xrange [-3:3]
set yrange [-3:3]
set ylabel "r"
set term png
set output "./figures/orbitas.png"
m = "./data/datosoTL.dat"
set nokey
set grid polar
set title "Orbita"
set polar
plot m using 1:2 with lines
set term x11
