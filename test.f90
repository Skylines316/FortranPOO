program hairyBH
    use class_blackHole
    ! use auxiliarFunctions
    use particula_time_like
    implicit none
    real :: M, x_horizonte, r_horizonte
    type ( blackHole ) :: miBH
    type ( timeLike ) :: miTL

    miBH = blackHole ( 1, 12.52655373, 1.52)
    miTL = timeLike (-0.025, 2.6e-6)
    call potencial(miBH, miTL)
    M = masa (miBH)
    write(*,*) 'La masa del BH es', M
    call show_consts(miBH)
    x_horizonte = horizonte_hairy(miBH)
    r_horizonte = horizonte(miBH)
    print *, 'El horizonte del BH en coordenadas r es', x_horizonte
    print *, 'El horizonte del BH tipo Swarzschild', r_horizonte
end program hairyBH