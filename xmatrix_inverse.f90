program xmatrix_inverse
use kind_mod, only: dp
use linear_solve, only: inverse
implicit none
integer, parameter :: n = 3
real(kind=dp) :: a(n,n),ainv(n,n),chk(n,n)
integer :: i
character (len=*), parameter :: fmt_cr = "(a10,*(f8.4))"
call random_number(a)
ainv = inverse(a)
chk = matmul(a,ainv)
print fmt_cr,"a =",a
print fmt_cr,"ainv =",ainv
print*,"matmul(a,ainv)"
do i=1,n
   write (*,"(*(f8.4))") chk(i,:)
end do
end program xmatrix_inverse