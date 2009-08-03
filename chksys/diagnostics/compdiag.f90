! compdiag.f90 --
!     Program to analyse the behaviour of the compiler
!
program compdiag
    implicit none

    logical            :: exists

    !
    ! Do we need to do anything?
    !
    inquire( file = 'compdiag.complete', exist = exists )

    if ( exists ) then
        stop
    endif

    !
    ! Analyse the compiler output or prepare a new test program
    !
    inquire( file = 'check.out', exist = exists )

    write(*,*) 'Check: ', exists
    if ( exists ) then
        call analyse
    else
        call prepare
    endif

contains

subroutine prepare
    integer :: count
    integer :: casecnt
    integer :: ierr
    logical :: found
    character(len=132) :: line

    found = .false.

    open( 10, file = 'compdiag.count', status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        read( 10, * ) casecnt
        close( 10, status = 'delete' )
    else
        casecnt = 1
    endif

    open( 10, file = 'compdiag.inp', status = 'old' )

    count = 0
    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        if ( index( line, '@desc' ) == 1 ) then
            count = count + 1

            if ( count == casecnt ) then
                found = .true.
                exit
            endif
        endif
    enddo

    ! We have found a new test case:
    ! copy it to file

    write(*,*) 'Found: ', found, count, casecnt

    if ( found ) then
        open( 21, file = 'compdiag.test' )
        open( 22, file = 'check.f90' )

        write( 21, * ) count
        write( 21, '(a)' ) line

        do
            read( 10, '(a)', iostat = ierr ) line
            if ( ierr /= 0 ) exit

            if ( index( line, '@desc' ) == 1 ) then
                exit
            endif

            if ( index( line, '@' ) == 1 ) then
                write( 21, '(a)' ) trim(line)
            else
                write( 22, '(a)' ) trim(line)
            endif
        enddo

        close( 21 )
        close( 22 )
    else
        open( 21, file = 'compdiag.complete' )
        close( 21 )
    endif
end subroutine prepare

subroutine analyse
    integer :: count
    integer :: ierr
    character(len=132) :: line

    open( 10, file = 'compdiag.test' )
    open( 11, file = 'check.out' )
    open( 21, file = 'compdiag.log', position = 'append' )

    ! TODO: everything

    read( 10, * ) count

    open( 22, file = 'compdiag.count' )
    write( 22, * ) count + 1
    close( 22 )

    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        write( 21, '(a)' ) trim(line)
    enddo
    do
        read( 11, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        write( 21, '(a)' ) trim(line)
    enddo

    close( 11, status = 'delete' )

end subroutine analyse
end program compdiag
