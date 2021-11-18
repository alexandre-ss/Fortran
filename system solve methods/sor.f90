program sor
    implicit none

    integer :: i, j, k, n
    double precision :: w, omega
    double precision, dimension (:), allocatable :: x , b
    double precision, dimension (:,:), allocatable :: a 

    n = 3

    allocate(x(n))
    allocate(b(n))
    allocate(a(n,n))

    a(1,1) = 1.d0
    a(1,2) = 1.d0
    a(1,3) = 1.d0

    a(2,1) = 1.d0
    a(2,2) = 2.d0
    a(2,3) = 2.d0

    a(3,1) = 2.d0
    a(3,2) = 1.d0
    a(3,3) = 3.d0

    b(1) = 6.d0
    b(2) = 9.d0
    b(3) = 11.d0

    !! initial guess
    x(1) = 1.d0
    x(2) = 1.d0
    x(3) = 1.d0

    w = 1.5d0

    do k = 1, 50
        do i = i,n
            omega = 0.d0
            do j=1,n
                if(j.ne.i) then
                    omega = omega + a(i,j)*x(j) 
                end if
            end do
            x(i) = (1 - w)*x(i) + (w/(a(i,i)))*(b(i) - omega)
        end do
        write(*,*) x
    end do

end program sor