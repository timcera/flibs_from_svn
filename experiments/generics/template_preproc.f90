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
program template_preproc
    implicit none

    character(len=200) :: srcline
    character(len=100) :: filename, include_file
    integer            :: ierr, lun, lunout, lunincl, luntemp
    integer            :: includeno
    logical            :: In_definition, in_module


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


    in_module = .false.
    includeno = 0

    do
        read( lun, '(a)', iostat = ierr ) srcline
        if ( ierr /= 0 ) then
            exit
        endif

        !
        ! Handle the start of a module, subroutine, function of submodule
        ! (for now, only "module")
        !
        if ( starts_with( srcline, 'module' ) ) then
            in_module = .true.
            includeno = includeno +1
            write( include_file, '(a,i0,2a)' ) '_' , includeno, '_', trim(filename)
            open( newunit = lunincl, file = include_file )
            write( lunout, '(a)' ) trim(srcline)
            write( lunout, '(a,a)' ) 'include ', trim(include_file)

        elseif ( starts_with( srcline, 'end module' ) ) then
            in_module = .false.
            close( lunincl )
            write( lunout, '(a)' ) 'END MODULE:'
            call copy_temp_file( luntemp, lunout )
            write( lunout, '(a)' ) trim(srcline)

        elseif ( starts_with( srcline, 'use_template' ) ) then
            !
            ! Do we have a use_template statement? If so, handle it separately
            !
            write( lunout, '(a)' ) 'USE_TEMPLATE'
            write( *, '(a)' ) 'USE_TEMPLATE -- main'
            call handle_template_usage( lunout, srcline, .false. )

        elseif ( starts_with( srcline, 'template' ) ) then
            !
            ! Do we have a template definition? If so, handle it separately
            !
            write( lunout, '(a)' ) 'TEMPLATE'
            call handle_template( lun, srcline )
        else
            !
            ! Any other line
            !
            write( lunout, '(a)' ) trim(srcline)
        endif
    enddo
contains
! starts_width --
!     Determine if a line starts with a certain word
!
! Arguments:
!     line              Line to be examined
!     word              Word that may be the first
!
logical function starts_with( line, word )
     character(len=*), intent(in) :: line
     character(len=*), intent(in) :: word

     character(len=len(word)+1)   :: first_word
     character(len=len(word)+1)   :: second_word
     integer                      :: ierr

     !
     ! If only one word ...
     !
     read( word, *, iostat = ierr ) first_word, second_word
     if ( ierr /= 0 ) then
         first_word = '-----'
         read( line, *, iostat = ierr ) first_word

         starts_with = first_word == word .and. ierr == 0
     else
         first_word = '-----'
         read( line, *, iostat = ierr ) first_word, second_word

         starts_with = (trim(first_word) // ' ' // trim(second_word)) == word .and. ierr == 0
     endif
end function starts_with

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
    logical                      :: In_definition, in_type

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
        if ( ierr /=0 ) then
            exit
        endif

        if ( in_definition ) then
            !
            ! Ignore "implicit" statements
            !

            if ( starts_with( srcline, "implicit" ) ) then
                cycle
            endif

            !
            ! "use" statements are collected in "lunuse"
            ! other lines in the definitions section in "lunuse"
            ! all lines in the contains section in "luncont"
            !
            ! Be aware of any "use_template" statements
            !
            if ( starts_with( srcline, "use" ) ) then
                write( lunuse, '(a)' ) trim(srcline)
                cycle
            endif

            if ( starts_with( srcline, "use_template" ) ) then
                call handle_template_usage( lundef, srcline, .true. )
                cycle
            endif

            if ( starts_with( srcline, "type" ) ) then
                in_type = .true.
            endif
            if ( starts_with( srcline, "endtype" ) .or. starts_with( srcline, "end type" ) ) then
                in_type = .false.
            endif

            if ( starts_with( srcline, "contains" ) .and. .not. in_type ) then
                in_definition = .false.
            endif

            if ( starts_with( srcline, "endtemplate" ) .or. starts_with( srcline, "end template" ) ) then
                write(*,*) 'Closing intermediate files - ', trim(template_name)
                close( lunuse  )
                close( luncont )
                close( lundef  )
                exit
            endif

            !
            ! Write out the source line
            !
            if ( in_definition ) then
                write( lundef, '(a)' ) trim(srcline)
            endif
        else
            if ( starts_with( srcline, "use_template" ) ) then
                write(*,*) '>>> use 2'
                call handle_template_usage( lundef, srcline, .true. )
                cycle
            endif

            if ( starts_with( srcline, "endtemplate" ) .or. starts_with( srcline, "end template" ) ) then
                write(*,*) 'Closing intermediate files - ', trim(template_name)
                close( lunuse  )
                close( luncont )
                close( lundef  )
                exit
            endif

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
!     tmpl_use       Current source line, the use statement
!     in_template    True if nested template - hack!
!
subroutine handle_template_usage( lun, tmpl_use, in_template )
    integer, intent(in)          :: lun
    character(len=*), intent(in) :: tmpl_use
    logical, intent(in)          :: in_template

    character(len=1)             :: dummy
    character(len=100)           :: template_name
    integer                      :: ierr1, ierr2, ierr3, lunuse, luncont, lundef
    logical                      :: exist1, exist2, exist3

    !
    ! For the moment: ignore the substution list
    !
    write( lunout, * ) 'USE_TEMPLATE:', trim(tmpl_use), ' -- ', in_template
    read( tmpl_use, * ) dummy, template_name

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
    ! Quick hack: nested templates require more handling!
    !
    write(*,*) 'In template: ', in_template, lunincl
    if ( .not. in_template ) then
        write( lunincl, '(a)' ) 'INCLUDE:'
        call copy_temp_file( lunuse, lunincl )
    endif

    !
    ! For the moment: copy the "definition" intermediate template file
    ! - to the processed source file
    !
    write( lun, '(a,i0)' ) 'DEFINITION:', lun
    call copy_temp_file( lundef, lun )
    write(*,*) 'template usage done'
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

end program template_preproc
