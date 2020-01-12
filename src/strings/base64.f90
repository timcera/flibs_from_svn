! base64.f90 --
!     Encode and decode to and from base64 strings
!
!     Implementation based on the Wikipedia page - https://en.wikipedia.org/wiki/Base64.
!     No variants supported.
!
module base64
    implicit none

    private
    public::  encode, decode

    character(len=1), dimension(0:63) :: tobase64 = &
        ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z', &
         'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z', &
         '0','1','2','3','4','5','6','7','8','9','+','/' ]

contains

! encode --
!     Encode a string to base64
!
! Arguments:
!     string            The string of bytes that should be encoded
!     encoded_string    The resulting base64 string
!
! Note:
!     The entire input string is encoded, any trailing blanks
!     will be considered significant
!
subroutine encode( string, encoded_string )
    character(len=*), intent(in)               :: string
    character(len=:), allocatable, intent(out) :: encoded_string

    integer                       :: i, p, length, length3
    integer                       :: binvalue
    integer                       :: bin6, remainder
    character(len=3)              :: part

    !
    ! Calculate the length of the resulting string
    !
    length = 4 * ( (len(string) + 2) / 3 )

    if ( allocated(encoded_string) ) then
        deallocate( encoded_string )
    endif
    allocate( character(len=length) :: encoded_string )

    !
    ! The easy part: take three characters at a time
    !
    length3 = 3 * ( len(string) / 3 )
    p = 0
    do i = 1, length3, 3
        part      = string(i:i+2)
        binvalue  = iachar(part(1:1)) * 256 * 256 + iachar(part(2:2)) * 256 + iachar(part(3:3))

        bin6      = binvalue / 64 / 64 / 64
        remainder = mod( binvalue, 64 * 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        bin6      = remainder / 64 / 64
        remainder = mod( remainder, 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        bin6      = remainder / 64
        remainder = mod( remainder, 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        p = p + 1
        encoded_string(p:p) = tobase64(remainder)
    enddo

    !
    ! Now the rest
    !
    if ( len(string) == length3 + 1 ) then
        i         = length3 + 1
        part      = string(i:i)
        binvalue  = iachar(part(1:1)) * 256 * 256

        bin6      = binvalue / 64 / 64 / 64
        remainder = mod( binvalue, 64 * 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        bin6      = remainder / 64 / 64
        remainder = mod( remainder, 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        p = p + 1
        encoded_string(p:p+1) = '=='
    endif

    if ( len(string) == length3 + 2 ) then
        i         = length3 + 1
        part      = string(i:i+1)
        binvalue  = iachar(part(1:1)) * 256 * 256 + iachar(part(2:2)) * 256

        bin6      = binvalue / 64 / 64 / 64
        remainder = mod( binvalue, 64 * 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        bin6      = remainder / 64 / 64
        remainder = mod( remainder, 64 * 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        bin6      = remainder / 64
        remainder = mod( remainder, 64 )

        p = p + 1
        encoded_string(p:p) = tobase64(bin6)

        p = p + 1
        encoded_string(p:p) = '='
    endif

end subroutine encode

! dencode --
!     Decode a base64 string
!
! Arguments:
!     encoded_string    The base64 string to be decoded
!     string            The resulting string
!     error             If the string contains invalid characters
!
! Note:
!     The base64 string may contain blanks - these are ignored
!     It may or may not contain padding characters
!
subroutine decode( encoded_string, string, error )
    character(len=*), intent(in)               :: encoded_string
    character(len=:), allocatable, intent(out) :: string
    logical, intent(out)                       :: error

    character(len=:), allocatable              :: clean_copy
    integer                                    :: i, k, length, length8, p
    integer                                    :: binvalue, bin8, remainder

    error = .false.

    call check_base64( encoded_string, error )
    if ( error ) then
        return
    endif
    !
    ! Create a clean copy of the encoded string
    !
    allocate( character(len=len(encoded_string)) :: clean_copy )

    k = 0
    do i = 1,len(encoded_string)
        if ( encoded_string(i:i) /= ' ' ) then
            k = k + 1
            clean_copy(k:k) = encoded_string(i:i)
        endif
        if ( clean_copy(k:k) == '=' ) then
            k = k - 1
            exit
        endif
    enddo

    !
    ! Set the rest of the string to NUL characters
    !
    do i = k+1,len(encoded_string)
        clean_copy(i:i) = achar(0)
    enddo

    length = k

    !
    ! Four encoded characters become three decoded characters
    !
    length8 = (6 * length) / 8
    allocate( character(len=length8) :: string )

    p       = 0
    do i = 1,length,4
        binvalue = frombase64( clean_copy(i:i)     ) * 64 * 64 * 64 + &
                   frombase64( clean_copy(i+1:i+1) ) * 64 * 64      + &
                   frombase64( clean_copy(i+2:i+2) ) * 64           + &
                   frombase64( clean_copy(i+3:i+3) )

        bin8      = binvalue / 256 / 256
        remainder = mod( binvalue, 256 * 256 )

        p = p + 1
        string(p:p) = achar(bin8)

        bin8      = remainder / 256
        remainder = mod( remainder, 256 )

        p = p + 1
        string(p:p) = achar(bin8)

        p = p + 1
        string(p:p) = achar(remainder)
    enddo


end subroutine decode

! frombase64
!     Calculate the value that corresponds to the base64 character
!
! Arguments:
!     char           Character to be converted
!
integer function frombase64( char )
    character(len=*), intent(in) :: char

    if ( char == '+' ) then
        frombase64 = 62
    elseif ( char == '/' ) then
        frombase64 = 63
    elseif ( iachar(char) < iachar('A') ) then
        frombase64 = 52 + iachar(char) - iachar('0')
    elseif ( iachar(char) <= iachar('Z') ) then
        frombase64 = iachar(char) - iachar('A')
    else
        frombase64 = 26 + iachar(char) - iachar('a')
    endif
end function frombase64

! check_base64
!     Check that all characters are acceptable for base64
!
! Arguments:
!     string         String to be checked
!     error          True if anything does not match
!
subroutine check_base64( string, error )
    character(len=*), intent(in) :: string
    logical, intent(out)         :: error

    integer                      :: i

    error = .false.
    do i = 1,len(string)
        if ( index( '0123456789 +/=', string(i:i) ) > 0 ) then
            cycle
        endif
        if ( iachar(string(i:i)) >= iachar('A') .and. iachar(string(i:i)) <= iachar('z') ) then
            cycle
        endif

        error = .true.
        exit
    enddo
end subroutine check_base64

end module base64
