! ipc_file.f90 --
!     Simple method for inter-process communication via file I/O
!
! ipc_file --
!     Module for IPC via file I/O
!
! NOTES:
!     Start in separet DOS-boxes
!     Order of starting important - right now!
!
module ipc_file
    use dflib ! Hm, compiler-dependency here!

    implicit none

    type ipc_comm
        character(len=20) :: me          ! Identifying string for calling program
        character(len=20) :: connection  ! Id for connecting program
        character(len=20) :: tag         ! Tag for the data (type)
        integer           :: id          ! Numerical identfier
        integer           :: lun
    end type ipc_comm
contains

type(ipc_comm) function ipc_open( me )
    character(len=*) :: me

    ipc_open%me = me
end function ipc_open

subroutine ipc_send_start( comm, dest, tag, id, lun )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: tag
    integer          :: id
    integer          :: lun

    lun = 10

    write(*,*) 'Send started'
    !
    ! Clean the send file
    !
    open( lun, file = trim(comm%me) // "-" // trim(dest) // ".send", &
        form = 'unformatted' )
    write( lun )
    close( lun )

    !
    ! Open the data file
    !
    open( lun, file = trim(comm%me) // "-" // trim(dest) // ".data", &
        form = 'unformatted' )

    comm%lun        = lun
    comm%connection = dest
    comm%tag        = tag
    comm%id         = id
end subroutine

subroutine ipc_send_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    integer          :: ierr

    close( comm%lun )
    open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".send", &
        form = 'unformatted' )
    write( comm%lun ) comm%me, comm%connection, comm%tag, comm%id
    close( comm%lun )

    do
        open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".recv", &
            form = 'unformatted', status = 'old', iostat = ierr )
        write(*,*) 'Send finishing: ', ierr
        if ( ierr == 0 ) then
            read( comm%lun, iostat = ierr ) okay
            close( comm%lun )
            write(*,*) 'Send finishing: ', okay
            if ( ierr == 0 .and. okay ) exit
        endif
        call sleepqq( 1000 )
    enddo
    open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".recv", &
        form = 'unformatted', status = 'old', iostat = ierr )
    write( comm%lun )
    close( comm%lun )
    write(*,*) 'Send finished'

end subroutine

subroutine ipc_receive_start( comm, src, tag, id, lun )
    type(ipc_comm)   :: comm
    character(len=*) :: src
    character(len=*) :: tag
    integer          :: id
    integer          :: lun

    character(len=20) :: src_
    character(len=20) :: dest_
    character(len=20) :: tag_
    integer           :: id_

    integer           :: ierr

    lun = 10

    comm%lun        = lun
    comm%connection = src
    comm%tag        = tag

    write(*,*) 'Receive started'
    !
    ! Clean the receive file
    !
!    open( lun, file = trim(src) // "-" // trim(comm%me) // ".recv", &
!        form = 'unformatted' )
!    write( lun )
!    close( lun )

    !
    ! Open the send file
    !
    do
        open( lun, file = trim(src) // "-" // trim(comm%me) // ".send", &
            form = 'unformatted', status = 'old', iostat = ierr, &
            position = 'rewind' )
        write(*,*) 'Receive starting: ', ierr
        if ( ierr == 0 ) then
            read( lun, iostat = ierr ) src_, dest_, tag_, id_
            close( lun )
            write(*,*) 'Receive starting: (read) ', ierr
            write(*,*) 'Receive starting: (read) ', src_, src
            write(*,*) 'Receive starting: (read) ', dest_, comm%me
            write(*,*) 'Receive starting: (read) ', tag_, tag

            if ( ierr == 0 .and. &
                 src_ == src .and. dest_ .eq. comm%me .and. &
                 tag_ == tag ) then
               exit
            endif
        endif

        call sleepqq( 1000 ) ! One second
    enddo

    comm%id         = id_
    id              = id_

    open( lun, file = trim(src) // "-" // trim(comm%me) // ".data", &
        form = 'unformatted', status = 'old', iostat = ierr )

end subroutine

subroutine ipc_receive_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    close( comm%lun )
    open( comm%lun, file = trim(comm%connection) // "-" // trim(comm%me) // ".send", &
        form = 'unformatted' )
    write( comm%lun )
    close( comm%lun )

    open( comm%lun, file = trim(comm%connection) // "-" // trim(comm%me) // ".recv", &
        form = 'unformatted' )
    write( comm%lun ) .true.
    close( comm%lun )
    write(*,*) 'Receive finished'

end subroutine

end module ipc_file

! Test program:
!     Send messages from one instantation to the other
!
program test_ipc_file

    use ipc_file

    implicit none

    type(ipc_comm)    :: comm

    integer           :: i
    integer           :: id
    integer           :: lun
    real              :: v
    character(len=20) :: myid
    character(len=20) :: task
    character(len=20) :: keyword
    character(len=20) :: value
    character(len=20) :: tag
    character(len=20) :: connection

    !
    ! Read the information from the command-line:
    ! - Id of the instance
    ! - Task and which one to connect to
    !
    do i = 1,2
        read( *, * ) keyword, value
        write( *, * ) 'Read: ', keyword, value
        select case (keyword)
            case('id')
                myid = value
            case('send', 'receive')
                task       = keyword
                connection = value
        end select
    enddo

    comm = ipc_open( myid )
    !
    ! Depending on the task we either send a number or receive it
    ! and print it
    !
    if ( task == 'send' ) then
        do i = 1,10
            id = i
            write( *, * ) myid, ': sending ... ', v, id
            call random_number( v )
            call ipc_send_start( comm, connection, 'number', id, lun )
            write( lun ) v
            write( *, * ) myid, ': sent ', v, id
            call ipc_send_finish( comm )
        enddo
    else
        tag = 'number'
        do i = 1,10
            id = i
            write( *, * ) myid, ': receiving ... '
            call ipc_receive_start( comm, connection, tag, id, lun )
            read( lun ) v
            write( *, * ) myid, ': received ', v, id
            call ipc_receive_finish( comm )
        enddo
    endif
end program