module class_blackHole
    use class_funciones
    use NewtonRaphson
    implicit none
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    ! type blackHole
    !     real :: alpha, eta, nu
    ! end type blackHole
contains
    subroutine show_consts(bh)
        type ( blackHole ), intent(in) :: bh
        print*, "c = ", c
        print*, "G = ", G
        print*, "G/c2 = ", G/c**2
        print*, "f= ", f(1.4,bh)
        call test(bh)
    end subroutine show_consts

    function masa (bh) result ( M )
        ! Tomar en cuenta que la masa aqui es GM/c2
        implicit none
        real :: M
        type ( blackHole ), intent(in) :: bh
        if (bh%alpha > 0) then
            M = c ** 2 / G * (3 * bh%eta ** 2 + bh%alpha) / bh%eta ** 3 / 6
            print *, 'alpha es mayor que 0'
        else if (bh%alpha < 0) then
            M = -c ** 2 / G * (3 * bh%eta ** 2 + bh%alpha) / bh%eta ** 3 / 6
            print *, 'alpha es menor que 0'
        else
            print *, 'alpha no puede ser 0'
        endif
    end function masa

    function horizonte_hairy_x (bh) result (x)
        !Ubicacion del horizonte pero en coordenadas hairy
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: x_inicial, epsF, eps, x
        integer :: limLoop
        if (bh%alpha > 0) then
            x_inicial = 0.00004
            eps = 0.0001
            epsF = 0.0001
            limLoop = 15
            x = solveF (x_inicial, bh, eps, epsF, limLoop)
        elseif (bh%alpha < 0) then
            x_inicial = 26
            eps = 0.0001
            epsF = 0.0001
            limLoop = 15
            x = solveF (x_inicial, bh, eps, epsF, limLoop)
        endif
    end function horizonte_hairy_x

end module
