program polynomial_interpolation
    implicit none
    integer :: n,g,i,j,k
    double precision :: sum
    double precision, dimension (:,:), allocatable :: a, atransp, gram
    double precision, dimension (:), allocatable :: t,y,x,b

    n = 5
    g = 4 !degree of a polynomial

    allocate(atransp(g+1,n))
    allocate(gram(g+1,g+1))
    allocate(t(n))
    allocate(y(n))
    allocate(b(g+1))
    allocate(x(g+1))
    
    t(1) = 0.523d0
    t(2) = 0.696d0
    t(3) = 0.785d0
    t(4) = 0.955d0
    t(5) = 1.048d0

    y(1) = 0.866d0
    y(2) = 0.821d0
    y(3) = 0.708d0
    y(4) = 0.644d0
    y(5) = 0.480d0

    allocate(a(n,g+1))



    do i = 1,n
        do j = 1,g+1
            a(i,j) = t(i)**(j-1)
        end do
    end do

    !transposta
    do i = 1,g+1
        do j= 1,n
            atransp(i,j) = a(j,i)
        end do
    end do

    !gram matrix

    do i = 1, g+1
        do j = 1, g+1
            sum = 0.d0
            do k = 1,n
                sum = sum + atransp(i,k)*a(k,j)
            end do
            gram(i,j) = sum
        end do  
    end do
    !vetor solucao
    do i =  1,g+1
        sum = 0.d0
        do k = 1,n
            sum = sum + atransp(i,k)*y(k)
        end do
        b(i) = sum
    end do
    
    call solve_system(a,y,x,g+1)
    write(*,*) x

    deallocate(a)
    deallocate(atransp)
    deallocate(gram)
    deallocate(t)
    deallocate(y)
    deallocate(b)
    deallocate(x)
contains

subroutine solve_system(a,b,x,n)
    implicit none

    integer :: i, j, k, n
    double precision :: sum
    double precision, dimension (n) :: x , b
    double precision, dimension (n,n) :: a 

    
    x = 0.d0 !initial guess
    do k = 1,100000 !i assume that after n iterations the output will be my the result which i am looking for
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


end program polynomial_interpolation