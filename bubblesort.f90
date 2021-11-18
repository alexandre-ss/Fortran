program ex03
    implicit none

    real, dimension(:), allocatable :: angulo, sorted, teste
    integer :: i, N
    N = 25
    allocate(angulo(0:N))
    allocate(sorted(0:N))

    allocate(teste(0:N))

    open(unit=1234, file='teste.dat', status="unknown")
        do i=0,N
            read(1234,*) angulo(i) ,sorted(i)
        end do    
    close(unit=1234)

    teste = bubblesort(N, sorted)
    
    deallocate(teste)
    deallocate(angulo)
    deallocate(sorted)
    
contains 
    function bubblesort(tamanho, vetor)
        implicit none
        integer :: tamanho, i, j
        real, dimension(tamanho) :: vetor, bubblesort
        real :: aux

        do i=1,tamanho
            j = i
            do while ( (j .gt. 0) .and. (vetor(j-1) .gt. vetor(j)))
                aux = vetor(j)
                vetor(j) = vetor(j-1)
                vetor(j-1) = aux
                j = j - 1
            end do
        end do  

        open(unit=2608, file='seno_ordenado.dat',status="unknown") 
        do i = 1,25
                write(2608,*) vetor(i)
        end do
        close(2608)
        
        return
        

    end function bubblesort

end program ex03