program nrsyst
    implicit none

    double precision, dimension (:), allocatable :: x,y, nold
    integer :: n,k

    n =3
    allocate(x(n))
    allocate(y(n))
    allocate(nold(n))

    x(1) = 0.1d0 !initial guess
    x(2) = 0.1d0
    x(3) = 0.1d0

    nold = 1.d0
    do k = 0, 100
        call solve_system(df(x,n),f(x,n),y,n)
        x = x - y !vectorial operation
        nold = x - nold
        write (*,*) x
    end do
    write (*,*) x

    deallocate(x)
    deallocate(y)
contains 

    function f(x,n)
        implicit none
        integer :: n
        double precision, dimension (n) :: f,x

        f(1) = 10*(x(2) - x(1))
        f(2) = 16*x(1) - x(1)*x(3)
        f(3) = -2.66*x(3) + x(1)*x(2)
        return 
    end function f


    function df(x,n)
        implicit none
        integer :: n
        double precision, dimension (n) :: x
        double precision, dimension (n,n) :: df

        !partial derivative to calculate the jacobian matrix
        df(1,1) =  -10.d0
        df(1,2) =  10.d0
        df(1,3) =  0.d0

        df(2,1) = 16 - x(3)
        df(2,2) = 0.d0
        df(2,3) = -x(1)

        df(3,1) = x(2)
        df(3,2) = x(1)
        df(3,3) = -2.66

        return 
    end function df

    subroutine solve_system(a,b,x,n)
        implicit none

        integer :: i, j, k, n
        double precision :: sum
        double precision, dimension (n) :: x , b
        double precision, dimension (n,n) :: a 

        
        x = 0.d0 !initial guess
        do k = 1,100 !i assume that after 100 iterations the output will be my the result which i am looking for
            do i=1,n
                sum = 0.d0
                do j = 1,n
                    if(j.ne.i) then !unless i am not at the diagonal, i will sum all the terms of the current row
                        sum = sum + a(i,j)*x(j)
                    end if
                end do
                x(i) = (b(i) - sum)/a(i,i)
            end do
        end do
        return
    end subroutine solve_system

end program nrsyst