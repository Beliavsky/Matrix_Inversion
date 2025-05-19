module linear_solve
use kind_mod, only: dp
implicit none
public :: krout,inverse,hat_matrix,equicorr,inverse_equicorr_off_diag, &
   inverse_equicorr
contains
subroutine KROUT(MO, N, M, A, KA, B, KB, IERR)
!-----------------------------------------------------------------------
!  CROUT PROCEDURE FOR INVERTING MATRICES AND SOLVING EQUATIONS
!-----------------------------------------------------------------------
!  A IS A MATRIX OF ORDER N WHERE N IS GREATER THAN OR EQUAL TO 1.
!  IF MO = 0 THEN THE INVERSE OF A IS COMPUTED AND STORED IN A.
!  IF MO IS NOT 0 THEN THE INVERSE IS NOT COMPUTED.

!  IF M IS GREATER THAN 0 THEN B IS A MATRIX HAVING N ROWS AND M COLUMNS.
!  IN THIS CASE AX = B IS SOLVED AND THE SOLUTION X IS STORED IN B.
!  IF M=0 THEN THERE ARE NO EQUATIONS TO BE SOLVED.
!  N.B. B is passed as a VECTOR not as a matrix.

!  KA = THE LENGTH OF THE COLUMNS OF THE ARRAY A
!  KB = THE LENGTH OF THE COLUMNS OF THE ARRAY B (IF M > 0)

!  IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS. WHEN
!  THE ROUTINE TERMINATES IERR HAS ONE OF THE FOLLOWING VALUES ...
!     IERR =  0   THE REQUESTED TASK WAS PERFORMED.
!     IERR = -1   EITHER N, KA, OR KB IS INCORRECT.
!     IERR =  K   THE K-TH PIVOT ELEMENT IS 0.

!-----------------------------------------------------------------------

! Adapted from the routine KROUT in the NSWC Math. Library by Alan Miller
! Latest revision - 3 August 1998

integer, intent(in)                            :: MO ! if M0 = 0 then the inverse of A is computed and stored in A. Otherwise the inverse is not computed.
integer, intent(in)                            :: N  ! # of columns of A
integer, intent(in)                            :: M
real (kind=DP), intent(in out), dimension(:,:) :: A     ! a(ka,n)
integer, intent(in)                            :: KA
real (kind=DP), intent(in out), dimension(:)   :: B
integer, intent(in)                            :: KB
integer, intent(out)                           :: IERR

!     INDEX IS AN ARRAY OF DIMENSION N-1 OR LARGER THAT IS USED BY THE
!     ROUTINE FOR KEEPING TRACK OF THE ROW INTERCHANGES THAT ARE MADE.
!     IF MO IS NOT 0 THEN THIS ARRAY IS NOT NEEDED.

!     TEMP IS AN ARRAY OF DIMENSION N OR LARGER THAT IS USED WHEN A
!     IS INVERTED.  IF MO IS NOT 0 THEN THIS ARRAY IS NOT NEEDED.

integer, allocatable, dimension(:)         :: INDX
real (kind=DP), allocatable, dimension(:)  :: TEMP

integer        :: I, J, JP1, K, KJ, KM1, KP1, L, LJ, MAXB, NJ, NMJ, NMK, NM1, &
                  ONEJ
real (kind=DP) :: D, DSUM, P, T
real (kind=DP), parameter :: ZERO = 0.0_DP, ONE = 1.0_DP

if (N < 1 .or. KA < N) then
  IERR = -1
  return
end if
if (M > 0 .and. KB < N) then
  IERR = -1
  return
end if

IERR = 0
if (N < 2) then

!                      CASE WHEN N = 1

  D = A(1,1)
  if (D == ZERO) then
    IERR = N
    return
  end if
  if (MO == 0) then
    A(1,1) = ONE / D
  end if

  if (M <= 0) then
    return
  end if
  MAXB = KB*M
  do KJ = 1,MAXB,KB
    B(KJ) = B(KJ)/D
  end do
  return
end if

!                      General case

if (MO == 0) then
  allocate( INDX(N-1), TEMP(N) )
end if

NM1 = N - 1
do K = 1,NM1
  KP1 = K + 1

!               SEARCH FOR THE K-TH PIVOT ELEMENT

  P = abs(A(K,K))
  L = K
  do I = KP1,N
    T = abs(A(I,K))
    if (P >= T) then
      cycle
    end if
    P = T
    L = I
  end do
  if (P == ZERO) then
!                  K-TH PIVOT ELEMENT IS 0
    IERR = K
    return
  end if
  P = A(L,K)
  if (MO == 0) then
    INDX(K) = L
  end if
  if (K /= L) then
!                  INTERCHANGING ROWS K AND L
    do J = 1,N
      T = A(K,J)
      A(K,J) = A(L,J)
      A(L,J) = T
    end do
    if (M > 0) then
      KJ = K
      LJ = L
      do J = 1,M
        T = B(KJ)
        B(KJ) = B(LJ)
        B(LJ) = T
        KJ = KJ + KB
        LJ = LJ + KB
      end do
    end if
  end if
!                  COMPUTE THE K-TH ROW OF U
  if (K <= 1) then
    do J = KP1,N
      A(K,J) = A(K,J)/P
    end do
  else
    do J = KP1,N
      DSUM = A(K,J) - dot_product( A(K,1:KM1), A(1:KM1,J) )
      A(K,J) = DSUM / P
    end do
  end if
!               COMPUTE THE (K+1)-ST COLUMN OF L
  do I = KP1,N
    DSUM = A(I,KP1) - dot_product( A(I,1:K), A(1:K,KP1) )
    A(I,KP1) = DSUM
  end do
  KM1 = K
end do
!                 CHECK THE N-TH PIVOT ELEMENT
if (A(N,N) == ZERO) then
  IERR = N
  return
end if
!                 SOLVING THE EQUATION LY = B
if (M > 0) then
  MAXB = KB*M
  do ONEJ = 1,MAXB,KB
    KJ = ONEJ
    B(KJ) = B(KJ)/A(1,1)
    do K = 2,N
      KJ = KJ + 1
      DSUM = B(KJ)
      KM1 = K - 1
      LJ = ONEJ
      do L = 1,KM1
        DSUM = DSUM - A(K,L)*B(LJ)
        LJ = LJ + 1
      end do
      B(KJ) = DSUM / A(K,K)
    end do
  end do
!                 SOLVING THE EQUATION UX = Y
  do NJ = N,MAXB,KB
    KJ = NJ
    do NMK = 1,NM1
      K = N - NMK
      LJ = KJ
      KJ = KJ - 1
      DSUM = B(KJ)
      KP1 = K + 1
      do L = KP1,N
        DSUM = DSUM - A(K,L)*B(LJ)
        LJ = LJ + 1
      end do
      B(KJ) = DSUM
    end do
  end do
end if
!               REPLACE L WITH THE INVERSE OF L
if (MO /= 0) then
  return
end if
do J = 1,NM1
  A(J,J) = ONE / A(J,J)
  JP1 = J + 1
  do I = JP1,N
    DSUM = dot_product( A(I,J:I-1), A(J:I-1,J) )
    A(I,J) = -DSUM / A(I,I)
  end do
end do
A(N,N) = ONE / A(N,N)
!           SOLVE UX = Y WHERE Y IS THE INVERSE OF L
do NMK = 1,NM1
  K = N - NMK
  KP1 = K + 1
  do J = KP1,N
    TEMP(J) = A(K,J)
    A(K,J) = ZERO
  end do
  do J = 1,N
    DSUM = A(K,J) - dot_product( TEMP(KP1:N), A(KP1:N,J) )
    A(K,J) = DSUM
  end do
end do
!                    COLUMN INTERCHANGES
do NMJ = 1,NM1
  J = N - NMJ
  K = INDX(J)
  if (J == K) then
    cycle
  end if
  do I = 1,N
    T = A(I,J)
    A(I,J) = A(I,K)
    A(I,K) = T
  end do
end do
end subroutine KROUT
!
function inverse(a) result(ainv)
! return inverse of square matrix a
real(kind=dp), intent(in) :: a(:,:)                    ! matrix to inverted
real(kind=dp)             :: ainv(size(a,1),size(a,2)) ! inverse of a
integer                   :: ierr,n
real(kind=dp)             :: b(0)
n = size(a,1)
if (size(a,2) /= n) then
   ainv = 0.0_dp
   return
end if
ainv = a
call krout(mo=0,n=n,m=0,a=ainv,ka=n,b=b,kb=0,ierr=ierr)
if (ierr /= 0) then
   print*,"returning from krout, ierr =",ierr," STOPPING"
   error stop
end if
end function inverse
!
function hat_matrix(x) result(xhat)
! return the hat matrix of x, defined as x*(x'*x)^(-1)*x'
real(kind=dp), intent(in) :: x(:,:)                    ! matrix for which hat computed
real(kind=dp)             :: xhat(size(x,1),size(x,1)) ! hat matrix of x
xhat = matmul(matmul(x,inverse(matmul(transpose(x),x))),transpose(x))
end function hat_matrix

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
