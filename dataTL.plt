set xlabel "theta"
set rrange [0:1.5]
set grid polar
set ylabel "r"
set term png
set output "./figures/orbitas.png"
m = "./data/datosNull.dat"
set nokey
set grid polar
set title "Orbita"
set polar
plot m using 1:2 with lines
set term x11
