module particula_time_like
    use class_blackHole
    use class_funcionesBH
    use class_funcionesTL
    use auxiliarFunctions
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
contains

    subroutine  potencial(bh,tl)
        implicit none
        real :: i,r,U,step
        character(20) :: filename
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        write(filename,'(a,a)') './data/datos','.dat'
        open(1,file=filename)
        step=0.00001
        if (bh%alpha < 0) then
            i=1.0
            do while (i <100)
                U = U_potencial(i,bh,tl)
                r = omega(i,bh)
                write(1,*)
                i=i+step
            enddo
            close(1)
            call system ('gnuplot -p data.plt')
        elseif (bh%alpha > 0) then
            do while(i < 1)
                U = U_potencial(i,bh,tl)
                r = omega(i,bh)
                write(1,*) r,U
                i=i+step
            enddo
            close(1)
            call system ('gnuplot -p data.plt')
        else
            print *, 'Alpha no puede ser 0'
        endif
    end subroutine potencial

    ! !buscar una manera mas eficiente de hallar los maximos y minos de un de esa funcion
    ! function maximo(bh,tl) result(r)
    !     implicit none
    !     real, dimension(2) :: r
    !     real :: x_inicial, eps, epsF, x_max, U_max
    !     integer :: limLoop
    !     type ( blackHole ), intent(in) :: bh
    !     type ( timeLike ), intent(in) :: tl
    !     x_inicial = horizonte_hairy_x(bh)
    !     eps = 1e-7
    !     epsF = 1e-7
    !     limLoop = 20
    !     x_max = solveDiffU(x_inicial, bh, tl, eps, epsF, limLoop)
    !     U_max = omega(x_max,bh)*f(x_max, bh)*(1+tl%J **2*c**2/omega(x_max,bh))-1
    !     r=(/x_max, U_max/)
    ! end function maximo

    function U_nulo(bh,tl) result(r)
        implicit none
        real, dimension(2) :: r
        real :: x_inicial, eps, epsF, x_nul, U_nul
        integer :: limLoop
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        x_inicial = horizonte_hairy_x(bh)
        eps = 1e-7
        epsF = 1e-7
        limLoop = 20
        x_nul = solveU(x_inicial, bh, tl, eps, epsF, limLoop)
        U_nul = U_potencial(x_nul, bh, tl)
        r=(/x_nul, U_nul/)
    end function U_nulo

    function cond_init(r, bh, tl) result(r_init)
        implicit none
        real, dimension(2) :: r_init
        real :: xIni, eps, epsF, r, x_radio, x_prima
        integer :: limLoop
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        if (bh%alpha > 0) then
            xIni= 0.83
            eps = 1e-9
            epsF = 1e-9
            limLoop = 20
            x_radio = solveOmega_r(xIni, r, bh, eps, epsF, limLoop)
            x_prima = ((tl%energia-U_potencial(x_radio, bh, tl))/(bh%eta**2*tl%J**2*c**2))**0.5
            r_init =(/x_radio, x_prima/)
        elseif (bh%alpha < 0) then
            xIni= 1.22
            eps = 1e-9
            epsF = 1e-9
            limLoop = 20
            x_radio = solveOmega_r(xIni, r, bh, eps, epsF, limLoop)
            x_prima = ((tl%energia-U_potencial(x_radio, bh, tl))/(bh%eta**2*tl%J**2*c**2))**0.5
            r_init =(/x_radio, x_prima/)
        else
            print *, 'alpha no puede ser 0'
        endif
    end function cond_init

end module
