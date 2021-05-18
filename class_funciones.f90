module class_funciones
    type blackHole
        real :: alpha, eta, nu
    end type blackHole

contains
    subroutine test(bh)
        implicit none
        type ( blackHole ), intent(in) :: bh
        print *, 'alpha: ', bh%alpha
        print *, 'nu: ', bh%nu
        print *, 'eta: ', bh%eta
    end subroutine test

    function omega(x, bh) result (O)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: x, nu, eta, O
        O = nu ** 2 * x ** (nu - 1) / eta ** 2 / (x ** nu - 1) ** 2
    end function omega

    function f(x, bh) result (fun)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: x, alpha, nu, eta, fun, O
        nu = bh%nu
        eta = bh%eta
        alpha = bh%alpha
        O = nu ** 2 * x ** (nu - 1) / eta ** 2 / (x ** nu - 1) ** 2
        fun =x/O + alpha * (1 / (nu ** 2 - 4) - x ** 2 * (1 + x ** (-nu) / (nu - 2) - x ** nu / (nu + 2)) / nu ** 2)
    end function f

    function diff_omega(x, nu, eta) result (dO)
        implicit none
        real :: x, nu, eta, dO
        dO = -x**(nu-2)*nu**2*(nu-1+x**nu*(1+nu))/((x**nu-1)**3*eta**2)
    end function diff_omega

    function diff_f(x, bh) result (df)
        implicit none
        type ( blackHole ), intent(in) :: bh
        real :: x, alpha, nu, eta,df
        nu = bh%nu
        eta = bh%eta
        alpha = bh%alpha
        df = x ** (1 - nu) * (x**nu-1)*((x**nu-1)*alpha+eta**2*(nu-2+x**nu*(nu+2)))/nu**2
    end function diff_f
end module