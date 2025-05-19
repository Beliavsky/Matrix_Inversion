# Matrix_Inversion
Inversion of a real matrix and the solution of a set of linear equations using Crout's method, calling [Fortran code by Alan Miller](https://jblevins.org/mirror/amiller/). Compile with<br> 
`gfortran kind.f90 linear_solve.f90 xmatrix_inverse.f90`.

# Application to Portfolio Optimization
A correlation matrix of dimension `n` where all the off-diagonal elements have value `r` has an inverse matrix whose diagonal elements equal

`(1 + (n-2)*r) / ((1-r) * (1 + (n-1)*r))`

and off-diagonal elements equal

`r / ((r-1) * (1 + (n-1)*r)) `

Given the expected returns `mu` and covariances `cov` of assets, when taking positions `w` the expected portfolio return is `mu' * w`, the variance is `w' * cov * w`, and optimal portfolio weights are proportional to `cov^(-1) * mu`. The program below specifies expected returns for 3 assets and studies how the optimal portfolio weights depend on the average correlation.

```fortran
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
```
Output:
```
expected asset returns:       2.000       1.000       1.000

        corr          w1          w2          w3    mean_ret      sd_ret     mean/sd
       0.000       0.500       0.250       0.250       1.500       0.612       2.449
       0.100       0.556       0.222       0.222       1.556       0.683       2.277
       0.200       0.625       0.187       0.187       1.625       0.754       2.155
       0.300       0.714       0.143       0.143       1.714       0.828       2.070
       0.400       0.833       0.083       0.083       1.833       0.908       2.018
       0.500       1.000       0.000       0.000       2.000       1.000       2.000
       0.600       0.833      -0.083      -0.083       1.500       0.742       2.023
       0.700       0.714      -0.143      -0.143       1.143       0.542       2.108
       0.800       0.625      -0.188      -0.188       0.875       0.377       2.320
       0.900       0.556      -0.222      -0.222       0.667       0.228       2.928
```
For the case of uncorrelated assets, twice as much weight is given to asset 1 than the other  2 assets, since its expected return is twice as high. As correlation increases, the weights given to assets 2 and 3 decrease, since they have lower returns and are less effective at diversifying asset 1. For correlation of 0.5 asset 1 gets weight 1.0 and the other assets weight 0.0. As correlation increases still further, the optimal portfolio goes long asset 1 but now bets against assets 2 and 3 (shorts them), since doing so hedges the position in asset 1 and reduces risk. One see that this boosts the Sharpe ratio (the last column, labeled `mean/sd`). What hedge funds are supposed to do is go long and short assets to maximize the Sharpe ratio.

An individual investor may be unable or unwilling to take short positions and may impose a long-only constraint. This results in a quadratic programming problem that can be solved by  the [quadprog](https://github.com/loiseaujc/QuadProg) package of @loiseaujc.
