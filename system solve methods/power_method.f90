program power_method

    implicit none

    integer :: i, j, k, m, n
    double precision :: norma, normasum, max, err
    double precision, dimension (:), allocatable :: b, temp
    double precision, dimension (:,:), allocatable :: mat

    n = 3

    allocate(temp(n))
    allocate(b(n))
    allocate(mat(n,n)) 

    mat(1,1) = 0.d0
    mat(1,2) = -3.d0/25.d0
    mat(1,3) = -2.d0/25.d0
    !mat(1,4) = 3.055

    mat(2,1) = -1.d0/10.d0
    mat(2,2) = 0.d0
    mat(2,3) = -1.d0/10.d0
    !mat(2,4) = 5.4342

    mat(3,1) = 1.d0/20.d0
    mat(3,2) = -5.d0/20.d0
    mat(3,3) = 0.d0
    !mat(3,4) = -0.46759

    !mat(4,1) = -3.7347
    !mat(4,2) = -2.447
    !mat(4,3) = -1.3122
    !mat(4,4) = -2.6939


    b(1) = 1.d0
    b(2) = 1.d0
    b(3) = 1.d0
    !b(4) = 1.d0

    do m = 1,50
        do i = 1,n
            temp(i) = 0.d0
            do j = 1,n
                temp(i) = temp(i) + mat(i,j)*b(j)
            end do
        end do

        normasum = 0
        do k = 1,n
            normasum = normasum + temp(k)*temp(k)
        end do

        norma = dsqrt(normasum)     
        b = temp/norma
    end do

    write(*,*) "dominant eigenvalue: ", norma
    write(*,*) "eigenvalue of dominant eigenvalue: ", b 

    deallocate(temp)
    deallocate(b)
    deallocate(mat) 

end program power_method