module auxiliarFunctions
use class_funcionesBH

contains
    function solveF(xIni, bh, eps, epsF, limLoop) result(xFin)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: xIni, xFin, eps, epsF, error
        integer :: iter, limLoop
        iter = 0
        do while (iter < limLoop)
            xFin = xIni -f(xIni, bh)/diff_f(xIni, bh)
            error = abs(xIni-xFin)
            if (error < eps) then
                print *, 'La raiz de la funcion es: ' ,xFin
                exit
            elseif (abs(f(xFin, bh)) < epsF) then 
                print *, 'La raiz de la funcion es: ', xFin
                exit
            end if
            xIni=xFin
            iter = iter + 1
        end do

        if (iter > limLoop) then
            print *, 'No se encontro la solución convergente'
        endif

        print *, 'El numero de iteraciones fue de: ', iter
        print *, 'El valor del error es de: ', error

    end function solveF

    ! function linspace(xIni, xFin, points) result (x)
    !     implicit none
    !     integer :: points, i, step
    !     real :: xFin, xIni
    !     real , dimension(points) :: x
    !     step = (xFin-xIni)/points
    !     i=1
    !     do while (i <= points)
    !         x(i) = xIni
    !         xIni = xIni + step
    !         i = i + 1
    !     enddo
    ! end function


end module