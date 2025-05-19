program xequicorr
! test inverse_equicorr
use kind_mod    , only: dp
use linear_solve, only: equicorr, inverse_equicorr
use util_mod    , only: display
implicit none
integer, parameter :: n = 3
real(kind=dp) :: r, xmat(n,n), xinv(n,n)
r = 0.0_dp
xmat = equicorr(n, r)
xinv = inverse_equicorr(n, r)
call display(xmat, title="x")
call display(xinv, title="x^-1")
call display(matmul(xmat,xinv), title="x * x^-1")
end program xequicorr
