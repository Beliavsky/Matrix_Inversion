module util_mod
use iso_fortran_env, only: output_unit
use kind_mod, only: dp
implicit none
private
public :: default, assert_equal, write_merge, split_string, display, &
   print_time_elapsed, read_words_line, str, print_table, exe_name, &
   join, seq, cbind
interface default
   module procedure default_int, default_real, default_logical, &
      default_character
end interface default
interface seq
   module procedure seq_stride, seq_unit_stride
end interface seq
interface cbind
   module procedure cbind_vec_vec, cbind_mat_vec, cbind_mat_mat
end interface cbind
interface display
   module procedure display_matrix, display_vector
end interface display
contains

elemental function default_int(x, xopt) result(y)
! return xopt if present, otherwise x
integer, intent(in) :: x
integer, intent(in), optional :: xopt
integer             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_int

elemental function default_real(x, xopt) result(y)
! return xopt if present, otherwise x
real(kind=dp), intent(in) :: x
real(kind=dp), intent(in), optional :: xopt
real(kind=dp)             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_real

elemental function default_logical(x, xopt) result(y)
! return xopt if present, otherwise x
logical, intent(in) :: x
logical, intent(in), optional :: xopt
logical             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_logical

elemental function default_character(x, xopt) result(y)
! return xopt if present, otherwise x
character (len=*), intent(in) :: x
character (len=*), intent(in), optional :: xopt
character (len=100) :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_character

subroutine assert_equal(k, kreq, msg)
! check that k == kreq
integer, intent(in) :: k, kreq
character (len=*), intent(in) :: msg
if (k /= kreq) then
   print "(a, i0, a, i0)", msg // " = ", k, ", must equal ", kreq
   stop
end if
end subroutine assert_equal

subroutine write_merge(tf, x, y, outu, fmt)
!> Writes either `x` or `y` to the specified output unit using the given format.
!! If `tf` is true, writes `x`; otherwise, writes `y`.
!! @param tf Logical condition determining whether to write `x` or `y`.
!! @param x The first character string to write if `tf` is true.
!! @param y The second character string to write if `tf` is false.
!! @param outu Optional output unit (defaults to a predefined output unit).
!! @param fmt Optional format specifier (defaults to "(a)").
logical, intent(in) :: tf
character (len=*), intent(in) :: x, y
integer, intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt
integer :: outu_
character (len=100) :: fmt_
outu_ = default(output_unit, outu)
if (present(fmt)) then
   fmt_ = fmt
else
   fmt_ = "(a)"
end if
if (tf) then
   write (outu_, fmt_) x
else
   write (outu_, fmt_) y
end if
end subroutine write_merge

!------------------------------------------------------------------
! Utility: split_string
!
! Splits the input string 'str' at each occurrence of the single-
! character delimiter 'delim' and returns the pieces in the allocatable
! array 'tokens'. To allocate each element (with deferred length)
! properly, we use the length of the input string.
!------------------------------------------------------------------
subroutine split_string(str, delim, tokens)
character(len=*), intent(in)           :: str
character(len=*), intent(in)           :: delim
character(:), allocatable, intent(out) :: tokens(:)
integer :: start, pos, i, count, n

n = len_trim(str)
if (n == 0) then
   allocate(character(len=0) :: tokens(1))
   tokens(1) = ""
   return
end if

! First pass: count tokens.
count = 0
start = 1
do
   pos = index(str(start:), delim)
   if (pos == 0) then
      count = count + 1
      exit
   else
      count = count + 1
      start = start + pos
   end if
end do

! Allocate tokens; each token gets the full length of the input.
allocate(character(len=n) :: tokens(count))

! Second pass: extract tokens.
start = 1
i = 1
do
   pos = index(str(start:), delim)
   if (pos == 0) then
      tokens(i) = adjustl(str(start:))
      exit
   else
      tokens(i) = adjustl(str(start:start+pos-2))
      start = start + pos
      i = i + 1
   end if
end do
end subroutine split_string

subroutine display_matrix(x, outu, fmt_r, fmt_header, title)
! print a matrix
real(kind=dp)    , intent(in)           :: x(:,:)
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_r, fmt_header, title
integer                                 :: i, outu_
character (len=100)                     :: fmt_r_
outu_  = default(output_unit, outu)
fmt_r_ = default("(*(1x,f10.4))", fmt_r)
if (present(fmt_header)) write(outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
do i=1,size(x,1)
   write(outu_,fmt_r_) x(i,:)
end do
end subroutine display_matrix

subroutine display_vector(x, outu, fmt_r, fmt_header, title)
! print a vector
real(kind=dp)    , intent(in)           :: x(:)
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_r, fmt_header, title
integer                                 :: i, outu_
character (len=100)                     :: fmt_r_
outu_  = default(output_unit, outu)
fmt_r_ = default("(*(1x,f10.4))", fmt_r)
if (present(fmt_header)) write(outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
do i=1,size(x)
   write(outu_,fmt_r_) x(i)
end do
end subroutine display_vector

subroutine print_time_elapsed(old_time, outu)
real(kind=dp), intent(in) :: old_time ! previously set by call cpu_time(old_time)
real(kind=dp)             :: tt
integer      , intent(in), optional :: outu
integer                             :: outu_
character (len=100) :: fmt_time_
outu_ = default(output_unit, outu)
call cpu_time(tt)
fmt_time_= "('time elapsed (s): ', f0.4)"
write (outu_, fmt_time_) tt - old_time
end subroutine print_time_elapsed

subroutine read_words_line(iu,words)
! read words from line, where the line has the # of words followed by the words
! n word_1 word_2 ... word_n
integer          , intent(in)               :: iu
character (len=*), intent(out), allocatable :: words(:)
integer :: ierr, nwords
character (len=10000) :: text
read (iu,"(a)") text
read (text, *) nwords
allocate (words(nwords))
read (text, *, iostat=ierr) nwords, words
if (ierr /= 0) then
   print*,"could not read ", nwords, " words from '" // trim(text) // "'"
   error stop
end if
end subroutine read_words_line

function str(i) result(text)
! convert integer to string
integer, intent(in) :: i
character (len=20) :: text
write (text,"(i0)") i
end function str

subroutine print_table(x, row_names, col_names, outu, &
   fmt_col_names, fmt_row, fmt_header, fmt_trailer)
! print a table with row and column names
real(kind=dp)    , intent(in) :: x(:,:) ! matrix to be printed
character (len=*), intent(in) :: row_names(:), col_names(:)
integer          , intent(in), optional :: outu ! output unit
character (len=*), intent(in), optional :: fmt_col_names, fmt_row, &
   fmt_header, fmt_trailer
integer                       :: i, n1, n2, outu_
character (len=*), parameter  :: msg="in print_table, "
character (len=100) :: fmt_col_names_, fmt_row_
n1 = size(x, 1)
n2 = size(x, 2)
call assert_equal(size(row_names), n1, msg // "size(row_names)")
call assert_equal(size(col_names), n2, msg // "size(col_names)")
fmt_col_names_ = default("(*(a12,:,1x))", fmt_col_names)
fmt_row_ = default("(a12, *(1x,f12.6))", fmt_row)
outu_ = default(output_unit, outu)
if (present(fmt_header)) write (outu_, fmt_header)
write (outu_, fmt_col_names_) "", (trim(col_names(i)), i=1,n2)
do i=1,n1
   write (outu_, fmt_row_) trim(row_names(i)), x(i,:)
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_table

function exe_name() result(xname)
! return the program name
character (len=1000) :: xname
call get_command_argument(0,xname)
xname = trim(xname)
end function exe_name

function join(words,sep) result(str)
! trim and concatenate a vector of character variables,
! inserting sep between them
character (len=*), intent(in)                                   :: words(:),sep
character (len=(size(words)-1)*len(sep) + sum(len_trim(words))) :: str
integer                                                         :: i,nw
nw  = size(words)
str = ""
if (nw < 1) then
   return
else
   str = words(1)
end if
do i=2,nw
   str = trim(str) // sep // words(i) 
end do
end function join

pure function seq_stride(first, last, stride) result(vec)
!! return an integer sequence from first through last
integer, intent(in) :: first, last, stride
integer, allocatable :: vec(:)
integer :: i, n, idiff
idiff = last - first
n = max(0, 1 + idiff/stride)
allocate (vec(n))
do i=1, n
   vec(i) = first + (i - 1) * stride
end do
end function seq_stride

pure function seq_unit_stride(first, last) result(vec)
!! return an integer sequence from first through last
integer, intent(in) :: first, last
integer, allocatable :: vec(:)
integer :: i, n
n = max(0, last - first + 1)
allocate (vec(n))
do i=1, n
   vec(i) = first + i - 1
end do
end function seq_unit_stride

pure function cbind_vec_vec(x,y) result(xy)
! return a matrix whose columns are x(:) and y(:)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n
n = size(x,1)
if (size(y) /= n) error stop "mismatched sizes in cbind"
xy = reshape([x, y], [n, 2])
end function cbind_vec_vec

pure function cbind_mat_vec(x,y) result(xy)
! append vector y(:) to matrix x(:,:)
real(kind=dp), intent(in) :: x(:,:), y(:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n1, n2
n1 = size(x,1)
if (size(y) /= n1) error stop "mismatched sizes in cbind"
n2 = size(x,2)
allocate (xy(n1, n2+1))
xy(:,:n2)  = x
xy(:,n2+1) = y 
end function cbind_mat_vec

pure function cbind_mat_mat(x,y) result(xy)
! append columns of y(:,:) to matrix x(:,:)
real(kind=dp), intent(in) :: x(:,:), y(:,:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n1, n2
n1 = size(x,1)
if (size(y,1) /= n1) error stop "mismatched sizes in cbind"
n2 = size(x,2)
allocate (xy(n1, n2+size(y,2)))
xy(:,:n2)  = x
xy(:,n2+1:) = y 
end function cbind_mat_mat

! function appended_char_vec(x, y) result(xy)
! character (len=*), intent(in) :: x(:)
! character (len=*), intent(in) :: y
! character (len=len(x)), allocatable :: xy(:)
!  
! end function appended_char_vec

end module util_mod
