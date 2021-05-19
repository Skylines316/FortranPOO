module class_blackHole
    use class_funcionesBH
    use auxiliarFunctions
    implicit none
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    ! type blackHole
    !     real :: alpha, eta, nu
    ! end type blackHole
contains
    subroutine show_consts(bh)
        type ( blackHole ), intent(in) :: bh
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
            eps = 0.00000000000000001
            epsF = 0.00000000000000000000000000000000000001
            limLoop = 15
            x = solveF (x_inicial, bh, eps, epsF, limLoop)
        elseif (bh%alpha < 0) then
            x_inicial = 26
            eps = 1e-15
            epsF = 1e-15
            limLoop = 15
            x = solveF (x_inicial, bh, eps, epsF, limLoop)
        endif
    end function horizonte_hairy_x

    function horizonte_hairy(bh) result(r)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: xHorizon, r
        xHorizon = horizonte_hairy_x(bh)
        r = omega(xHorizon, bh)**0.5
    end function horizonte_hairy

    function horizonte(bh) result(rSchwarzschild)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: rSchwarzschild
        rSchwarzschild = 2*G*masa(bh)/c**2
    end function horizonte

end module
