program newton

  implicit none

  integer :: i, N
  double precision, dimension(:), allocatable :: x

  N = 100
  allocate(x(0:N))
  
  x(0) = 1.0d0

  do i=0,N-1
     x(i+1) = x(i) - f(x(i))/flinha(x(i))
  end do

  do i=0,N
     write(*,*) i, x(i)
  end do
  
  deallocate(x)

contains

    function f(x)

        implicit none

        double precision :: f, x

        f = x-Exp(-x)

        return
    
    end function f

    function flinha(x)

        implicit none

        double precision :: flinha, x

        flinha = 1+Exp(x)

        return
    
    end function flinha
  
  
end program newton