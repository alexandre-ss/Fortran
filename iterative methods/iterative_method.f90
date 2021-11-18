program l02ex01 
    implicit none

    integer :: n
    double precision, dimension(0:1000) :: x
    double precision :: pi
    pi =  acos(-1.0)

    x(0) = 1.0d0
    do n=0,999
        x(n+1) = sqrt((-x(n)**3 - 6.d0*x(n) - 24.d0)/(-7.d0))
        write(*,*) x(n)
    end do
    
end program l02ex01