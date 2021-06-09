module particula_null
    use class_blackHole
    use class_funcionesBH
    use class_funcionesNull
    use auxiliarFunctions
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    real, parameter, private :: pi = 3.141592653589
contains

    subroutine  potencialNull(bh)
        implicit none
        real :: i,r,U,step
        character(20) :: filename
        type ( blackHole ), intent(in) :: bh
        write(filename,'(a,a)') './data/datos','.dat'
        open(1,file=filename)
        step=0.00001
        if (bh%alpha < 0) then
            i=1.0
            do while (i <100)
                U = U_potencialNull(i,bh)
                r = omega(i,bh)**(0.5)
                write(1,*) r, U
                i=i+step
            enddo
            close(1)
            call system ('gnuplot -p data.plt')
        elseif (bh%alpha > 0) then
            i=0
            do while(i < 1)
                U = U_potencialNull(i,bh)
                r = omega(i,bh)**(0.5)
                write(1,*) r,U
                i=i+step
            enddo
            close(1)
            call system ('gnuplot -p data.plt')
        else
            print *, 'Alpha no puede ser 0'
        endif
    end subroutine potencialNull

    subroutine orbitasNull(r_init, bh,b)
        implicit none
        character(30) :: filename
        integer :: s, i, b
        real :: h, endtheta, theta, x_n, y_n, r_n, k_0, l_0, k_1, l_1, k_2, l_2, k_3, l_3, x_n1, y_n1
        real ,dimension(3) :: r_init
        type ( blackHole ), intent(in) :: bh
        s=10000
        endtheta =1*pi
        h=endtheta/s
        theta = r_init(3)
        x_n = r_init(1)
        y_n = r_init(2)
        write(filename,'(a,i0.2,a)') './data/datosNull',b,'.dat'
        open(1,file=filename)
        do i=1,s
            r_n = omega(x_n,bh)**0.5
            write (1,*) theta, r_n

            k_0 = h*y_n
            l_0 = h*(-funHNull(x_n,bh))

            k_1 = h*(y_n+l_0/2)
            l_1 = h*(-funHNull(x_n+0.5*k_0,bh))

            k_2 = h*(y_n+l_1/2)
            l_2 = h*(-funHNull(x_n+0.5*k_1,bh))

            k_3 = h*(y_n+l_2)
            l_3 = h*(-funHNull(x_n+k_2,bh))

            y_n1 = y_n+(l_0+2*l_1+2*l_2+l_3)/6
            x_n1 = x_n+(k_0+2*k_1+2*k_2+k_3)/6

            y_n = y_n1
            x_n = x_n1
            if (ABS(theta) > endtheta) then
                exit
            endif

            if (theta < 0) then
                theta = theta -h
            else
                theta = theta + h
            endif
        enddo
        close(1)
        ! call system ('gnuplot -p dataTL.plt')
    end subroutine orbitasNull

    subroutine orbitasNullMult(b, bh)
        implicit none
        real :: args
        real, dimension(21) :: b
        real, dimension(3) :: r_init
        integer :: i
        type ( blackHole ), intent(in) :: bh
        type ( null ) :: nl
        do i=1,21
            nl = null (b(i))
            r_init = cond_initNull(1.5, bh, nl)
            call orbitasNull(r_init, bh, i)
        enddo
        end subroutine orbitasNullMult

    ! subroutine orbitasNullmult(r_init, cuadros, bh)
    !     implicit none
    !     character(20) :: filename
    !     character(50) :: comand
    !     real :: h, theta, x_n, y_n, r_n, k_0, l_0, k_1, l_1, k_2, l_2, k_3, l_3, x_n1, y_n1
    !     integer :: cuadros, j, i, s
    !     real ,dimension(3) :: r_init
    !     type ( blackHole ), intent(in) :: bh
    !     type ( timeLike ), intent(in) :: tl
    !     do j=1,cuadros
    !         s=10000
    !         h=j*50*pi/(s*cuadros)
    !         theta = r_init(3)
    !         x_n = r_init(1)
    !         y_n = r_init(2)
    !         write(filename,'(a,i0.4,a)') './temp/data',j,'.dat'
    !         open(1,file=filename)
    !         do i=1,s
    !             r_n = omega(x_n,bh)**0.5
    !             write (1,*) theta, r_n

    !             k_0 = h*y_n
    !             l_0 = h*(-funHNull(x_n,bh,tl))

    !             k_1 = h*(y_n+l_0/2)
    !             l_1 = h*(-funHNull(x_n+0.5*k_0,bh,tl))

    !             k_2 = h*(y_n+l_1/2)
    !             l_2 = h*(-funHNull(x_n+0.5*k_1,bh,tl))

    !             k_3 = h*(y_n+l_2)
    !             l_3 = h*(-funHNull(x_n+k_2,bh,tl))

    !             y_n1 = y_n+(l_0+2*l_1+2*l_2+l_3)/6
    !             x_n1 = x_n+(k_0+2*k_1+2*k_2+k_3)/6

    !             y_n = y_n1
    !             x_n = x_n1

    !             theta = theta + h
    !         enddo
    !         close(1)
    !         ! write (comand, '(a,i0.4,a)') 'echo "',j,'"'
    !         ! call system (comand)
    !         ! write(comand, '(a,i0,a)') 'gnuplot -e "prenum=',j,'" anim.plt'
    !         ! call system (comand)
    !     enddo
    ! end subroutine

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

    ! function U_nulo(bh,tl) result(r)
    !     implicit none
    !     real, dimension(2) :: r
    !     real :: x_inicial, eps, epsF, x_nul, U_nul
    !     integer :: limLoop
    !     type ( blackHole ), intent(in) :: bh
    !     type ( timeLike ), intent(in) :: tl
    !     x_inicial = horizonte_hairy_x(bh)
    !     eps = 1e-7
    !     epsF = 1e-7
    !     limLoop = 20
    !     x_nul = solveU(x_inicial, bh, tl, eps, epsF, limLoop)
    !     U_nul = U_potencial(x_nul, bh, tl)
    !     r=(/x_nul, U_nul/)
    ! end function U_nulo

    function cond_initNull (r_x, bh, nl) result(r_init)
        implicit none
        real, dimension(3) :: r_init
        real :: xIni, eps, epsF, r_x, r, x_radio, x_prima, theta
        integer :: limLoop
        type ( blackHole ), intent(in) :: bh
        type ( null ), intent(in) :: nl
        r = (r_x**2+nl%b**2)**0.5
        print *, r
        theta = ATAN(nl%b/r_x)
        xIni = 0.94
        eps = 1e-9
        epsF = 1e-9
        limLoop = 30
        x_radio = solveOmega_r(xIni, r, bh, eps, epsF, limLoop)
        x_prima = -SQRT((1/nl%b**2-f(x_radio, bh))/(bh%eta**2))
        r_init=(/x_radio, x_prima, theta/)
    end function cond_initNull



end module
