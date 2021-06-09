set rrange [0:1.5]
set grid polar
# unset tics
# unset raxis
# c=sprintf("%05d", b)
# num=sprintf("%04d", prenum)
set term png
set nokey
set title "Orbita"
set polar
# set output "./temp/Null".num.".png"
set output "./temp/Null.png"
plot "./data/datosNull01.dat" using 1:2 with lines title "b=-0.3", \
     "./data/datosNull02.dat" using 1:2 with lines title "b=-0.2", \
     "./data/datosNull03.dat" using 1:2 with lines title "b=-0.1", \
     "./data/datosNull04.dat" using 1:2 with lines title "b=0.1", \
     "./data/datosNull05.dat" using 1:2 with lines title "b=0.2", \
     "./data/datosNull06.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull07.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull08.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull09.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull10.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull11.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull12.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull13.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull14.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull15.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull16.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull17.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull18.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull19.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull20.dat" using 1:2 with lines title "b=0.3", \
     "./data/datosNull21.dat" using 1:2 with lines title "b=0.3"
set term x11