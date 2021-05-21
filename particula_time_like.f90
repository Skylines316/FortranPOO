module particula_time_like
    use class_blackHole
    use class_funcionesBH
    use class_funcionesTL
    use auxiliarFunctions
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    real, parameter, private :: pi = 3.141592653589
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
                write(1,*) r, U
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

    subroutine orbitasTL(r_init, bh, tl)
        implicit none
        character(20) :: filename
        integer :: s, i
        real :: h, theta, x_n, y_n, r_n, k_0, l_0, k_1, l_1, k_2, l_2, k_3, l_3, x_n1, y_n1
        real ,dimension(2) :: r_init
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        s=10000
        h=8.5*pi/s
        theta = 0.0
        x_n = r_init(1)
        y_n = r_init(2)
        write(filename,'(a,a)') './data/datosoTL','.dat'
        open(1,file=filename)
        do i=1,s
            r_n = omega(x_n,bh)**0.5
            write (1,*) theta, r_n

            k_0 = h*y_n
            l_0 = h*(-funH(x_n,bh,tl))

            k_1 = h*(y_n+l_0/2)
            l_1 = h*(-funH(x_n+0.5*k_0,bh,tl))

            k_2 = h*(y_n+l_1/2)
            l_2 = h*(-funH(x_n+0.5*k_1,bh,tl))

            k_3 = h*(y_n+l_2)
            l_3 = h*(-funH(x_n+k_2,bh,tl))

            y_n1 = y_n+(l_0+2*l_1+2*l_2+l_3)/6
            x_n1 = x_n+(k_0+2*k_1+2*k_2+k_3)/6

            y_n = y_n1
            x_n = x_n1

            theta = theta + h
        enddo
        close(1)
        call system ('gnuplot -p dataTL.plt')
    end subroutine orbitasTL

    subroutine orbitasTLanim(r_init, cuadros, bh, tl)
        implicit none
        character(20) :: filename
        character(50) :: comand
        real :: h, theta, x_n, y_n, r_n, k_0, l_0, k_1, l_1, k_2, l_2, k_3, l_3, x_n1, y_n1
        integer :: cuadros, j, i, s
        real ,dimension(2) :: r_init
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        do j=1,cuadros
            s=10000
            h=j*8.5*pi/(s*cuadros)
            theta = 0.0
            x_n = r_init(1)
            y_n = r_init(2)
            write(filename,'(a,i0,a)') './temp/data',j,'.dat'
            open(1,file=filename)
            do i=1,s
                r_n = omega(x_n,bh)**0.5
                write (1,*) theta, r_n

                k_0 = h*y_n
                l_0 = h*(-funH(x_n,bh,tl))

                k_1 = h*(y_n+l_0/2)
                l_1 = h*(-funH(x_n+0.5*k_0,bh,tl))

                k_2 = h*(y_n+l_1/2)
                l_2 = h*(-funH(x_n+0.5*k_1,bh,tl))

                k_3 = h*(y_n+l_2)
                l_3 = h*(-funH(x_n+k_2,bh,tl))

                y_n1 = y_n+(l_0+2*l_1+2*l_2+l_3)/6
                x_n1 = x_n+(k_0+2*k_1+2*k_2+k_3)/6

                y_n = y_n1
                x_n = x_n1

                theta = theta + h
            enddo
            close(1)
            write(comand, '(a,i0,a)') 'gnuplot -e "num=',j,'" anim.plt'
            call system (comand)
        enddo

    end subroutine

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
