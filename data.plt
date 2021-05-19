set xlabel "r"
set xrange [0:1]
set yrange [-0.1:0.1]
set ylabel "U"
m = "./data/datos.dat"
set terminal x11 0
set nokey
set grid
set title "Potencial"
plot m using 1:2 with lines