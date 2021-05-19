module auxiliarFunctions
use class_funcionesBH
use class_funcionesTL
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

    function solveOmega_r(xIni, r, bh, eps, epsF, limLoop) result(xFin)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: xIni, xFin, eps, epsF, error, r
        integer :: iter, limLoop
        iter = 0
        do while (iter < limLoop)
            xFin = xIni -(omega(xIni, bh)-r**2)/diff_omega(xIni, bh)
            error = abs(xIni-xFin)
            if (error < eps) then
                print *, 'La raiz de la funcion es: ' ,xFin
                exit
            elseif (abs(omega(xFin, bh)-r**2) < epsF) then 
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

    end function solveOmega_r

    function solveU(xIni, bh, tl, eps, epsF, limLoop) result(xFin)
        implicit none
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        real :: xIni, xFin, eps, epsF, error
        integer :: iter, limLoop
        iter = 0
        do while (iter < limLoop)
            xFin = xIni -U_potencial(xIni, bh, tl)/diff_U(xIni, bh, tl)
            error = abs(xIni-xFin)
            if (error < eps) then
                print *, 'La raiz de la funcion es: ' ,xFin
                exit
            elseif (abs(U_potencial(xFin, bh, tl)) < epsF) then 
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

    end function solveU

    !Crear una funcion que resuelva por newton raphson el diff_U, se necesitan diff_diff_Omega y el diff_diff_f

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