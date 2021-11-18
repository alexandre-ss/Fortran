program linear_regression

  implicit none

  integer :: N,G,i,j,k
  double precision :: sum
  double precision, dimension (:,:), allocatable :: a, atransp, gram
  double precision, dimension (:), allocatable :: t,y,x,b

  N = 6
  G = 1 !Grau do polinômio

  allocate(a(N,G+1))
  allocate(atransp(G+1,N))
  allocate(gram(G+1,G+1))
  allocate(t(N))
  allocate(y(N))
  allocate(b(G+1))
  allocate(x(G+1))

    t(1) = -0.08d0
    t(2) = 0.41d0
    t(3) = 0.93d0
    t(4) = 1.41d0
    t(5) = 2.01d0
    t(6) = 2.32d0
    t(7) = 2.86d0
    t(8) = 3.43d0
    t(9) = 4.07d0
    t(10) = 4.54d0

    y(1) = 1.11d0
    y(2) = 2.93d0
    y(3) = 3.39d0
    y(4) = 2.92d0
    y(5) = 2.52d0
    y(1) = 3.85d0
    y(2) = 6.15d0
    y(3) = 11.66d0
    y(4) = 14.86d0
    y(5) = 17.89d0
   
  !Building A matrix
  do i=1,N !N lines --> N points
     do j=1,G+1 !G+1 lines according with G adjustment
        a(i,j) = t(i)**(j-1)
     end do
  end do

  !building A transpose
  do i=1,G+1
     do j=1,N
        atransp(i,j) = a(j,i)
     end do
  end do

  !Building Gram matrix
  do i = 1,G+1
     do j = 1,G+1
        sum = 0.d0
        do k=1,N
           sum = sum + atransp(i,k)*a(k,j)
        end do
        gram(i,j) = sum
     end do
  end do

  !Building solution vector
  do i=1,G+1
     sum = 0.d0
     do k=1,N
        sum = sum + atransp(i,k)*y(k)
     end do
     b(i) = sum
  end do

  call solve_system(gram,b,x,G+1) !--> adjustment
  
  write(*,*) x

  deallocate(a)
  deallocate(atransp)
  deallocate(gram)
  deallocate(t)
  deallocate(y)
  deallocate(b)
  deallocate(x)
  
contains
  
 subroutine solve_system(a,b,x,N)

    implicit none
    
    integer :: i,j,k,N
    double precision :: sum
    double precision, dimension (N) :: x, b
    double precision, dimension (N,N) :: a

    x = 0.d0 !chute inicial
    do k = 1,6000000 !It may be changed
       do i=1,N !line loop
          sum = 0.d0
          do j=1,N !column loop
             if(j.ne.i) then !choosing not diagonal terms
                sum = sum + a(i,j)*x(j)
             end if
          end do
          x(i) = (b(i)-sum)/a(i,i)
       end do
    end do
    
    return
    
  end subroutine solve_system
  
end program linear_regression