program hairyBH
    use class_blackHole
    implicit none
    real :: M, x_horizonte
    type ( blackHole ) :: MiBH

    miBH = blackHole ( 1, 12.52655373, 1.52)
    M = masa (miBH)
    write(*,*) 'La masa del BH es', M
    call show_consts(miBH)
    x_horizonte = horizonte_hairy_x(miBH)
    print *, 'El horizonte del BH en coordenadas x es', x_horizonte
end program hairyBH