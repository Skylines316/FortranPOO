module class_funcionesNull
    use class_funcionesBH
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    type null
        real :: b
    end type null

contains
    function U_potencialNull(x,bh) result(U)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: x, U
        U = c**2*f(x, bh)
    end function U_potencialNull

    function funHNull(x, bh) result(H)
        implicit none
        real :: x, H
        type ( blackHole ), intent(in) :: bh
        H = diff_f(x,bh)/(2*bh%eta**2)
    end function funHNull
end module class_funcionesNull