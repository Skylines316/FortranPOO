program hairyBH
    use class_blackHole
    implicit none
    real :: M, x_horizonte, r_horizonte
    type ( blackHole ) :: MiBH

    miBH = blackHole ( 1, 12.52655373, 1.52)
    M = masa (miBH)
    write(*,*) 'La masa del BH es', M
    call show_consts(miBH)
    x_horizonte = horizonte_hairy(miBH)
    r_horizonte = horizonte(miBH)
    print *, 'El horizonte del BH en coordenadas r es', x_horizonte
    print *, 'El horizonte del BH tipo Swarzschild', r_horizonte
end program hairyBH