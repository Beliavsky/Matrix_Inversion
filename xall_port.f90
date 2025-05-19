module kind_mod
implicit none
private
public :: dp
integer, parameter :: dp = selected_real_kind(15, 307)
end module kind_mod

module linear_solve
use kind_mod, only: dp
implicit none
public :: equicorr, inverse_equicorr_off_diag, inverse_equicorr
contains

pure function equicorr(n, r) result(xmat)
! return a correlation matrix with equal off-diagonal elements
integer      , intent(in)  :: n ! dimension of correlation matrix
real(kind=dp), intent(in)  :: r ! off-diagonal correlation
real(kind=dp), allocatable :: xmat(:,:)
integer                    :: i
allocate (xmat(n, n), source = r)
do i=1,n
   xmat(i,i) = 1.0_dp
end do
end function equicorr

pure function inverse_equicorr_off_diag(n, r) result(y)
! value of off-diagonal elements of the inverse of an equicorrelation matrix
integer      , intent(in) :: n
real(kind=dp), intent(in) :: r
real(kind=dp)             :: y
y = r / ((r-1) * (1 + (n-1)*r)) 
end function inverse_equicorr_off_diag

pure function inverse_equicorr_diag(n, r) result(y)
! value of diagonal elements of the inverse of an equicorrelation matrix
integer      , intent(in) :: n
real(kind=dp), intent(in) :: r
real(kind=dp)             :: y
y = (1 + (n-2)*r) / ((1-r) * (1 + (n-1)*r))
end function inverse_equicorr_diag

pure function inverse_equicorr(n, r) result(xmat)
! return the inverse of a correlation matrix with equal off-diagonal elements
integer      , intent(in)  :: n ! dimension of correlation matrix
real(kind=dp), intent(in)  :: r ! off-diagonal correlation
real(kind=dp), allocatable :: xmat(:,:)
integer                    :: i
real(kind=dp)              :: ydiag
ydiag = inverse_equicorr_diag(n, r)
allocate (xmat(n, n), source = inverse_equicorr_off_diag(n, r))
do i=1,n
   xmat(i,i) = ydiag
end do
end function inverse_equicorr

end module linear_solve

program xequicorr_port
! find optimal portfolio as a function of correlation, given expected 
! returns and assuming all volatilities (standard deviations) are 1,
! so that the covariance matrix equals the correlation matrix
use kind_mod    , only: dp
use linear_solve, only: equicorr, inverse_equicorr
implicit none
integer :: ir
integer, parameter :: n = 3, nr = 10
real(kind=dp) :: r, xmat(n,n), xinv(n,n), mu(n), w(n), expected_ret, &
   sd_ret
mu = 1.0_dp
mu(1) = 2.0_dp ! stock 1 has higher expected return
print "('expected asset returns:',*(f12.3))", mu
print "(/,*(a12))", "corr", "w1", "w2", "w3", "mean_ret", "sd_ret", "mean/sd"
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
