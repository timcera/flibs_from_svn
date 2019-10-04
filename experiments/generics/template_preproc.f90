! template_preproc.f90 --
!     Preprocessor for "templates"
!
!     See my proposal for templates in Fortran
!
!     TODO: cast to lowercase
!
!     Hm, a module within a template?
!     What about a template for a single routine/function?
!
!     TODO: starts_with() -> first_word()
!
program template_preproc
    implicit none

    character(len=200) :: srcline
    character(len=100) :: filename, include_file
    integer            :: ierr, lun, lunout, lunincl, luntemp
    integer            :: includeno
    logical            :: in_definition, in_module, has_contains

    type substitution
        character(len=:), allocatable :: source
        character(len=:), allocatable :: target
    end type substitution

    !
    ! Open the source file and scan it for "templates"
    !
    call get_command_argument( 1, filename, status  = ierr )
    if ( ierr /= 0 ) then
        write(*,*) 'Usage: template_preproc srcfile'
        stop
    endif

    open( newunit = lun, file = filename, status = 'old', action = 'read', iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,*) 'Could not open ', trim(filename)
        stop
    endif
    open( newunit = lunout, file = "_" // filename )


    in_module    = .false.
    includeno    = 0
    has_contains = .false.

    do
        read( lun, '(a)', iostat = ierr ) srcline
        if ( ierr /= 0 ) then
            exit
        endif

        !
        ! Handle the start of a module, subroutine, function of submodule
        ! (for now, only "module")
        !
        select case( first_word(srcline) )
            case( 'module' )
                in_module = .true.
                includeno = includeno +1
                write( include_file, '(a,i0,2a)' ) '_' , includeno, '_', trim(filename)
                open( newunit = lunincl, file = include_file )
                write( lunout, '(a)' ) trim(srcline)
                write( lunout, '(a,a)' ) 'include ', trim(include_file)

            case( 'endmodule' )
                in_module = .false.
                close( lunincl )
                if ( .not. has_contains ) then
                    write( lunout, '(a)' ) 'contains ! added'
                endif
                call copy_temp_file( luntemp, lunout )
                write( lunout, '(a)' ) trim(srcline)

            case( 'contains' )
                has_contains = .true.
                write( lunout, '(a)' ) trim(srcline)

            case( 'use_template' )
                !
                ! Do we have a use_template statement? If so, handle it separately
                !
                call handle_template_usage( lunout, lunincl, srcline )

            case( 'template' )
                !
                ! Do we have a template definition? If so, handle it separately
                !
                call handle_template( lun, srcline )
            case default
                !
                ! Any other line
                !
                write( lunout, '(a)' ) trim(srcline)
        endselect
    enddo
contains

! first_word --
!     Read the first word from a line
!
! Arguments:
!     line              Line to be examined
!
! Note:
!     Slightly naïve, as it does not take care of variables and the like called "end"
!
function first_word( line )
    character(len=*), intent(in) :: line

    character(len=40)            :: first_word
    character(len=40)            :: first
    character(len=40)            :: second
    integer                      :: ierr

    read( line, *, iostat = ierr ) first

    if ( ierr /= 0 ) then
        first_word = ''
    elseif ( first == 'end' ) then
        read( line, *, iostat = ierr ) first, second

        if ( ierr /= 0 ) then
            first_word = first
        else
            first_word = trim(first) // trim(second)
        endif
    else
        first_word = first
    endif
end function first_word

! handle_template --
!     Handle the complete definition of the template:
!     - Copy the various parts to separate intermediate fles
!
! Arguments:
!     lun            LU-number of the source file
!     tmpl_start     Current source line, containing the name of the template
!
subroutine handle_template( lun, tmpl_start )
    integer, intent(in)          :: lun
    character(len=*), intent(in) :: tmpl_start

    character(len=200)           :: srcline
    character(len=1)             :: dummy
    character(len=40)            :: template_name
    integer                      :: ierr, lunuse, luncont, lundef
    logical                      :: in_definition, in_type

    write(*,*) trim(tmpl_start)
    read( tmpl_start, * ) dummy, template_name

    open( newunit = lunuse,  file = trim(template_name) // '.utpl' )  ! Any use-statements
    open( newunit = luncont, file = trim(template_name) // '.ctpl' )  ! Contains section
    open( newunit = lundef,  file = trim(template_name) // '.dtpl' )  ! Definition section

    in_definition = .true.
    in_type       = .false.

    !
    ! Copy the lines to the right pieces
    !
    do
        read( lun, '(a)', iostat = ierr ) srcline
        write(*,*) ierr, trim(srcline)
        if ( ierr /=0 ) then
            exit
        endif

        if ( in_definition ) then

            select case( first_word(srcline) )
                !
                ! Ignore "implicit" statements
                !
                case( "implicit" )
                    cycle

                !
                ! "use" statements are collected in "lunuse"
                ! other lines in the definitions section in "lunuse"
                ! all lines in the contains section in "luncont"
                !
                ! Be aware of any "use_template" statements
                !
                case( "use" )
                    write( lunuse, '(a)' ) trim(srcline)
                    cycle

                case( "use_template" )
                    call handle_template_usage( lundef, lunuse, srcline )
                    cycle

                case( "type" )
                    in_type = .true.

                case( "endtype" )
                    in_type = .false.

                case( "contains" )
                    if ( .not. in_type ) then
                        in_definition = .false.
                    endif

                case( "endtemplate" )
                    write(*,*) 'Closing intermediate files - ', trim(template_name)
                    close( lunuse  )
                    close( luncont )
                    close( lundef  )
                    exit
            endselect

            !
            ! Write out the source line
            !
            if ( in_definition ) then
                write( lundef, '(a)' ) trim(srcline)
            endif
        else
            select case( first_word(srcline) )
                case( "use_template" )
                    call handle_template_usage( lundef, lunuse, srcline )
                    cycle

                case( "endtemplate" )
                    write(*,*) 'Closing intermediate files - ', trim(template_name)
                    close( lunuse  )
                    close( luncont )
                    close( lundef  )
                    exit
            endselect

            !
            ! Write the source line
            !
            write( luncont, '(a)' ) trim(srcline)
        endif
    enddo
end subroutine handle_template

! handle_template_usage --
!     Handle the actual use of the template:
!     - Copy the various prepared parts into the source file
!
! Arguments:
!     lun            LU-number of the processed source file
!     lunusecaller   LU-number of the file for use statements in caller
!     tmpl_use       Current source line, the use statement
!
subroutine handle_template_usage( lun, lunusecaller, tmpl_use )
    integer, intent(in)             :: lun
    integer, intent(in)             :: lunusecaller
    character(len=*), intent(inout) :: tmpl_use

    character(len=1)             :: dummy
    character(len=100)           :: template_name
    integer                      :: ierr1, ierr2, ierr3, lunuse, luncont, lundef
    logical                      :: exist1, exist2, exist3

    type(substitution), dimension(:), allocatable :: pair

    !
    ! For the moment: ignore the substitution list
    !
    read( tmpl_use, * ) dummy, template_name

    !
    ! Parse the substitution list - if any
    !
    call parse_substitutions( tmpl_use, pair )
    write(*,*) 'Pairs: ', size(pair)

    !
    ! The template files must exist
    ! (Note: workaround for gfortran)
    !
    inquire( file = trim(template_name) // '.utpl', exist = exist1 )
    inquire( file = trim(template_name) // '.ctpl', exist = exist2 )
    inquire( file = trim(template_name) // '.dtpl', exist = exist3 )

    if ( exist1 .and. exist2 .and. exist3 ) then
        open( newunit = lunuse,  file = trim(template_name) // '.utpl', status = 'old', iostat = ierr1 )  ! Any use-statements
        open( newunit = luncont, file = trim(template_name) // '.ctpl', status = 'old', iostat = ierr2 )  ! Contains section
        open( newunit = lundef,  file = trim(template_name) // '.dtpl', status = 'old', iostat = ierr3 )  ! Definition section
    else
        write(*,*) 'Template intermediate files missing: ', trim(template_name)
        write(*,*) 'Processing stopped'
        stop
    endif

    luntemp = luncont ! Used with "end" statements

!    if ( ierr1 /= 0 .or. ierr2 /= 0 .or. ierr3 /= 0 ) then
!        write(*,*) ierr1, ierr2, ierr3
!        write(*,*) 'Template intermediate files missing: ', trim(template_name)
!        write(*,*) 'Processing stopped'
!        stop
!    endif

    !
    ! For the moment: copy the contents, if any, of the "use" intermediate template file
    ! - to the include file
    !
    call copy_temp_file( lunuse, lunusecaller )

    !
    ! For the moment: copy the "definition" intermediate template file
    ! - to the processed source file
    !
    call copy_temp_file( lundef, lun )
end subroutine handle_template_usage

! copy_temp_file --
!     Copy the contents of the indicated file to the destination file
!
! Arguments:
!     luninput          LU-number of the indicated input file
!     lunoutput         LU-number of the destination file
!
subroutine copy_temp_file( luninput, lunoutput )
    integer, intent(in) :: luninput
    integer, intent(in) :: lunoutput

    integer             :: ierr
    character(len=200)  :: line

    !!rewind( luninput, iostat = ierr ) ! Note: workaround for gfortran

    do
        read( luninput, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            exit
        endif

        write( lunoutput, '(a)' ) trim(line)
    enddo

end subroutine copy_temp_file

! parse_substitutions --
!     Parse the list of substitution pairs
!
subroutine parse_substitutions( srcline, pair )
    character(len=*), intent(inout)                            :: srcline
    type(substitution), dimension(:), allocatable, intent(out) :: pair

    type(substitution)                                         :: new_pair
    integer                                                    :: k, pos
    character(len=80)                                          :: string

    allocate( pair(0) )

    k = index( srcline, '!' )
    if ( k > 0 ) then
        srcline(k:) = ' '
    endif

    pos = 1
    do
         k = index( srcline(pos:), ',' )
         write(*,*) k, trim(srcline(pos:))

         if ( k == 0 ) then
             exit
         endif

         pos = pos + k

         k = index( srcline(pos:), '=>' )

         if ( k == 0 ) then
             write(*,*) 'Error in use_template line -- "a => b" substitution malformed'
             exit
         endif

         !
         ! We need this substring - it is the substitution string
         !
         k = pos + k - 1
         new_pair%target = trim(adjustl(srcline(pos:k-1)))

         pos = k + 2

         read( srcline(pos:), *, iostat = ierr ) string
         new_pair%source = trim(string)
         write(*,*) '>>', trim(srcline(pos:)), '<<  -- >>', new_pair%source, '<<'

         if ( ierr /= 0 ) then
             write(*,*) 'Error in use_template line -- "a => b" substitution malformed'
             exit
         endif

         !
         ! Add to the list
         !
         pair = [pair, new_pair]
     enddo

     do k = 1,size(pair)
         write(*,*) k, pair(k)%source, '-- ', pair(k)%target
     enddo
end subroutine parse_substitutions

end program template_preproc
