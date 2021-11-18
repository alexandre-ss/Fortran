program bubblesort
    implicit none

    real, dimension(:), allocatable :: angle, sorted, out
    integer :: i, N
    N = 25
    allocate(angle(0:N))
    allocate(sorted(0:N))

    allocate(out(0:N))

    open(unit=1234, file='values.dat', status="unknown")
        do i=0,N
            read(1234,*) angle(i) ,sorted(i)
        end do    
    close(unit=1234)

    out = bubblesort(N, sorted)
    
    deallocate(out)
    deallocate(angle)
    deallocate(sorted)
    
contains 
    function bubblesort(size, vector)
        implicit none
        integer :: size, i, j
        real, dimension(size) :: vector, bubblesort
        real :: aux

        do i=1,size
            j = i
            do while ( (j .gt. 0) .and. (vector(j-1) .gt. vector(j)))
                aux = vector(j)
                vector(j) = vector(j-1)
                vector(j-1) = aux
                j = j - 1
            end do
        end do  

        open(unit=2608, file='seno_ordenado.dat',status="unknown") 
        do i = 1,25
                write(2608,*) vector(i)
        end do
        close(2608)
        
        return    

    end function bubblesort

end program bubblesort