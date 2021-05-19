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

    function diff_U(x, bh, tl) result(dU)
        implicit none
        real :: x, dU
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        dU = diff_f(x,bh)*omega(x,bh)+diff_omega(x,bh) * f(x,bh)+(tl%J*c)**2*diff_f(x,bh)
    end function diff_U

    function funH(x, bh, tl) result(H)
        implicit none
        real :: x, H
        type ( blackHole ), intent(in) :: bh
        type ( timeLike ), intent(in) :: tl
        H = diff_U(x,bh,tl)/(2*tl%J**2*c**2*bh%eta**2)
    end function funH

    ! function diff_diff_U(x, bh, tl) result(dU)
    !     implicit none
    !     real :: x, dU
    !     type ( blackHole ), intent(in) :: bh
    !     type ( timeLike ), intent(in) :: tl
    !     dU = diff_f(x,bh)*omega(x,bh)+diff_omega(x,bh) * f(x,bh)+(tl%J*c)**2*diff_f(x,bh)
    ! end function diff_U

end module