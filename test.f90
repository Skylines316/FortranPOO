program hairyBH
    use class_blackHole
    implicit none
    real :: M
    type ( blackHole ) :: MiBH

    miBH = blackHole ( 1, 12.52655373, 1.52)
    M = masa (miBH)
    write(*,*) 'La masa del BH es', M
    call show_consts(miBH)
end program hairyBH