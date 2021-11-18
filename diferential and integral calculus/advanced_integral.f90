program adv_integral
    implicit none
    integer :: n, i
    double precision :: a,b,h,sum,t1,t2,t3,a1,a2,a3
    double precision, dimension (:), allocatable :: t
    n = 10
    sum = 0.d0

    allocate(t(0:n))

    ! a and b are the extremity of the integral
    a = 0.d0
    b = dacos(-1.d0)

    h = dabs(b-a)/dble(n)

    t1 = ( ((b-a)/2.d0) * (-dsqrt(3.d0/5.d0)) + ((b+a)/2.d0) )
    t2 = ( ((b-a)/2.d0) * (0.d0) + ((b+a)/2.d0) )
    t3 = ( ((b-a)/2.d0) * (dsqrt(3.d0/5.d0)) + ((b+a)/2.d0) )

    a1 = 5.d0/9.d0
    a2 = 8.d0/9.d0
    a3 = 5.d0/9.d0
    sum = (a1*f(t1) + a2*f(t2) + a3*f(t3)) * ((b-a)/2.d0)
    write(*,*) "integral's value: ", sum 
   deallocate(t)

    contains

        function f(x)
            implicit none
            double precision :: x,f
            
            f = x*cos(x)

            return
        end function f


end program adv_integral