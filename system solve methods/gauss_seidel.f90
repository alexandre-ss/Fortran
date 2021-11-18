program it_system_gs
    implicit none

    integer :: i, j, k, n
    double precision :: sum
    double precision, dimension (:), allocatable :: x , b
    double precision, dimension (:,:), allocatable :: a 

    ! iterative method to solve this kind of system

    ! a1*x1 + a1*x2 + a1*x3 = b1
    ! a2*x1 + a2*x2 + a2*x3 = b2
    ! a3*x1 + a3*x2 + a3*x3 = b3  
    ! a4*x1 + a4*x2 + a4*x3 = b4  

    n = 3

    allocate(x(n))
    allocate(b(n))
    allocate(a(n,n))

    a(1,1) = 1.d0
    a(1,2) = 1.d0
    a(1,3) = 1.d0
    !a(1,4) = 0.d0

    a(2,1) = 1.d0
    a(2,2) = 2.d0
    a(2,3) = 2.d0
    !a(2,4) = 0.d0

    a(3,1) = 2.d0
    a(3,2) = 1.d0
    a(3,3) = 3.d0
    !a(3,4) = -1.d0

    !a(4,1) = 0.d0
    !a(4,2) = 0.d0
    !a(4,3) = -1.d0
    !a(4,4) = 5.d0

    b(1) = 6.d0
    b(2) = 9.d0
    b(3) = 11.d0
    !b(4) = -11.d0

    !! initial guess

    x(1) = 0.d0
    x(2) = 0.d0
    x(3) = 0.d0


    do k = 1,200 !i assume that after 100 iterations the output will be my the result which i am looking for
        do i=1,n
            sum = 0.d0
            do j = 1,n
                if(j.ne.i) then !unless i am not at the diagonal, i will sum all the terms of the current row
                    sum = sum + a(i,j)*x(j)
                end if
            end do
            x(i) = (b(i) - sum)/a(i,i)
        end do
        write(*,*) k, x
    end do

    deallocate(x)
    deallocate(b)
    deallocate(a)

end program it_system_gs