program ajuste

  implicit none

  integer :: N,G,i,j,k
  double precision :: soma
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
   
  !Construção da matriz A
  do i=1,N !N linhas --> N pontos
     do j=1,G+1 !G+1 linhas de acordo com o grau G do ajuste
        a(i,j) = t(i)**(j-1)
     end do
  end do

  !Construção da transposta de A
  do i=1,G+1
     do j=1,N
        atransp(i,j) = a(j,i)
     end do
  end do

  !Construção da matriz de Gram
  do i = 1,G+1
     do j = 1,G+1
        soma = 0.d0
        do k=1,N
           soma = soma + atransp(i,k)*a(k,j)
        end do
        gram(i,j) = soma
     end do
  end do

  !Construir o vetor solução
  do i=1,G+1
     soma = 0.d0
     do k=1,N
        soma = soma + atransp(i,k)*y(k)
     end do
     b(i) = soma
  end do

  call resolve_sistema(gram,b,x,G+1) !--> ajuste
  !call resolve_sistema(a,y,x,G+1) !--> polinomio interpolador
  
  write(*,*) x

  deallocate(a)
  deallocate(atransp)
  deallocate(gram)
  deallocate(t)
  deallocate(y)
  deallocate(b)
  deallocate(x)
  
contains
  
 subroutine resolve_sistema(a,b,x,N)

    implicit none
    
    integer :: i,j,k,N
    double precision :: soma
    double precision, dimension (N) :: x, b
    double precision, dimension (N,N) :: a

    x = 0.d0 !chute inicial
    do k = 1,6000000 !CUIDADO -- PODE SER NECESSÁRIO MUDAR
       do i=1,N !loop nas linhas
          soma = 0.d0
          do j=1,N !loop nas colunas
             if(j.ne.i) then !escolher termos NÃO-DIAGONAIS
                soma = soma + a(i,j)*x(j)
             end if
          end do
          x(i) = (b(i)-soma)/a(i,i)
       end do
    end do
    
    return
    
  end subroutine resolve_sistema
  
end program ajuste