module class_funcionesTL
    use class_funcionesBH
    real, parameter, private :: c = 6.407e4
    real, parameter, private :: G = 39.309
    type timeLike
        real :: energia, J
    end type timeLike

contains
    function U_potencial(x,bh,tl) result(U)
        implicit none
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        real :: x, U
        U = omega(x,bh)*f(x,bh)*(1+tl%J**2*c**2/(omega(x,bh)))-1
    end function U_potencial

end module