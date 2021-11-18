program derivative
    implicit none

    double precision :: h
    integer :: i,n
    double precision, dimension(:), allocatable :: x, f_advanced, f_delayed
    n = 100

    allocate(x(0:n))
    allocate(f_advanced(0:n-1))
    allocate(f_delayed(1:n))


    h = 1.0/dble(n)
    !building sample points
    do i=0,n
        x(i) = i*h
    end do

    !derivative calculum
    do i = 0, n-1
        f_advanced(i) = (f(x(i + 1)) - f(x(i)))/h
        write(*,*) f_advanced(i)
    end do
    print *,"############"
    do i = 1,n
        f_delayed(i) = (f(x(i)) - f(x(i-1)))/h
        write(*,*) f_delayed(i)
    end do

    deallocate(x)
    deallocate(f_advanced)
    deallocate(f_delayed)

    contains 
        function f(x)
            implicit none
            double precision :: f,x
            
            f = x*dsin(x) !change this line using the function that you're calculating the derivative

            return
        end function f

end program derivative
