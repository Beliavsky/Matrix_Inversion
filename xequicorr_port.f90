program xequicorr_port
! find optimal portfolio as a function of correlation
use kind_mod    , only: dp
use linear_solve, only: equicorr, inverse_equicorr
use util_mod    , only: display
implicit none
integer :: ir
integer, parameter :: n = 3, nr = 10
real(kind=dp) :: r, xmat(n,n), xinv(n,n), mu(n), w(n), expected_ret, &
   sd_ret
mu = 1.0_dp
mu(1) = 2.0_dp
r = 0.0_dp
print "(*(a12))", "corr", "w1", "w2", "w3", "mean_ret", "sd_ret", "mean/sd"
do ir=1,nr
   r = (ir-1) * 0.1_dp
   xmat = equicorr(n, r)
   xinv = inverse_equicorr(n, r)
   w = matmul(xinv, mu)
   w = w / sum(abs(w))
   expected_ret = sum(w*mu)
   sd_ret = sqrt(sum(w * matmul(xmat,w)))
   print "(*(f12.3))", r, w, expected_ret, sd_ret, expected_ret/sd_ret
end do
end program xequicorr_port
