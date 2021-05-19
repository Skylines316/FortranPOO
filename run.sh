#! /bin/zsh
 
gfortran class_funciones.f90 -c
echo "Clase Funciones del Black Hole Bien"
gfortran NewtonRaphson.f90 -c
echo "Clase NewtonRaphson Bien"
gfortran hairyBH.f90 -c
echo "Clase hairy Bien"
gfortran class_funcionesTL.f90 -c
echo "Clase funciones de la particula Time Like bien"
gfortran particula_time_like.f90 -c
echo "Clase particula timeLike Bien"
gfortran class_funciones.o NewtonRaphson.o hairyBH.o class_funcionesTL.o particula_time_like.o test.f90 -o test.out
echo "Programa Compilado Correctamente"
