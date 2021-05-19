module particula_time_like
    use class_blackHole
    use class_funcionesBH
    use class_funcionesTL
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

end module
