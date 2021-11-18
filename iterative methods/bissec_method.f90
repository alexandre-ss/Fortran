program bissecao

  implicit none

  double precision, dimension(:), allocatable :: a, b, x
  integer :: k, N
  double precision :: err, tol_inter

  
  N = 100
  
  allocate(a(0:N))
  allocate(b(0:N))
  allocate(x(0:N))
  
  err = 2.222
  tol_inter = 1.d-3

  k = 0
  a(0) = -1.0d0
  b(0) = 1.0d0

  do while(err.ge.tol_inter)
     x(k) = (a(k)+b(k))*0.5d0
     if(f(x(k))*f(a(k)).lt.0.d0) then
        a(k+1) = a(k)
        b(k+1) = x(k)
     else
        a(k+1) = x(k)
        b(k+1) = b(k)
     end if
     err = dabs(b(k)-a(k))
     k = k + 1
  end do

  write(*,*) k-1, x(k-1), f(x(k-1))
  
  deallocate(a)
  deallocate(b)
  deallocate(x)
  
  contains

  function f(x)

    implicit none

    double precision :: f, x

    f = cos(x) - acos(-1.0)/x

    return
    
  end function f
end program bissecao