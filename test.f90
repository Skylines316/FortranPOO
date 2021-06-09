program hairyBH
    use class_blackHole
    ! use auxiliarFunctions
    ! use particula_time_like
    use particula_null
    implicit none
    real, dimension(21) :: b
    real , dimension(3) :: r_init
    integer :: i
    real :: M, x_horizonte, r_horizonte
    type ( blackHole ) :: miBH
    ! type ( timeLike ) :: miTL
    type ( null ) :: miNull

    miBH = blackHole ( 1, 12.52655373, 1.52)
    ! miTL = timeLike (-0.025, 2.6e-6)
    miNull = null (0.3)
    call potencialNull(miBH)
    M = masa (miBH)
    ! r_init = cond_init(0.4, miBH, miTL)
    r_init = cond_initNull(1.5, miBH, miNull)
    print *, 'las condiciones iniciales son', r_init
    do i = 1,21          
      b(i) = (i-11)*0.05     
    end do 
    print *, 'El array b es', b
    call orbitasNullMult(b, miBH)
    ! call orbitasTLanim(r_init, 1000, miBH, miTL)
    ! write(*,*) 'La masa del BH es', M
    ! call show_consts(miBH)
    ! x_horizonte = horizonte_hairy(miBH)
    ! r_horizonte = horizonte(miBH)
    ! print *, 'El horizonte del BH en coordenadas r es', x_horizonte
    ! print *, 'El horizonte del BH tipo Swarzschild', r_horizonte
end program hairyBH 