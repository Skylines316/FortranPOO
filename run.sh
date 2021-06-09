#! /bin/zsh

gfortran class_funciones.f90 -c
echo "Clase Funciones del Black Hole Bien"
gfortran class_funcionesTL.f90 -c
echo "Clase funciones de la particula Time Like bien"
gfortran class_funcionesNull.f90 -c
echo "Clase funciones de la particula Null bien"
gfortran NewtonRaphson.f90 -c
echo "Clase NewtonRaphson Bien"
gfortran hairyBH.f90 -c
echo "Clase hairy Bien"
gfortran particula_time_like.f90 -c
echo "Clase particula timeLike Bien"
gfortran particula_null.f90 -c
echo "Clase particula null Bien"
gfortran class_funciones.o NewtonRaphson.o hairyBH.o class_funcionesTL.o class_funcionesNull.o particula_time_like.o particula_null.o test.f90 -o test.out
echo "Programa Compilado Correctamente"
