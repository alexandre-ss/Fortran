program integral
    implicit none
    integer :: n, i
    double precision :: a,b,h,sum, sum1, sum2
    double precision, dimension (:), allocatable :: t
    n = 14

    allocate(t(0:n))

    ! a and b are the extremity of the integral
    a = 0.d0
    b = 3.d0

    h = dabs(b-a)/dble(n)

    do i = 0, n
        t(i) = a + i*h
    end do


    !sum = 0.d0
    !do i = 0, n-1
    !    sum = sum + f(t(i))*h 
    !end do

    sum = 0.d0
    !do i=0, n-1
    !    sum = sum + f(t(i))
    !end do
    !sum = h*(sum + (f(t(0)) + f(t(n)) )/2.d0)

    !""""""""""simpson's method"""""""""""""""
    !do i = 1, (n/2)-1
    !    sum1 = sum1 + f(t(2*i))
    !end do

    !do i = 1, (n/2)
    !    sum2 = sum2 + f(t(2*i - 1))
    !end do

    !sum = (h/3.d0)*(f(t(0)) + 2*sum1 + 4*sum2 + f(t(n)))


    write(*,*) "integral's value: ", sum 

    deallocate(t)

    contains

        function f(x)
            implicit none
            double precision :: x,f
            
            f = Exp(-x**2)*dcos(x)

            return
        end function f

end program integral