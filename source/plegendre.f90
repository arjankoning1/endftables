function plegendre(l, x)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Calculation of Legendre polynomial
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
  implicit none
  integer, parameter :: sgl = selected_real_kind(6,37)       ! single precision kind
  integer   :: l         ! multipolarity
  integer   :: i         ! level
  real(sgl) :: x         ! help variable
  real(sgl) :: pl0       !
  real(sgl) :: pl1       ! help variables
  real(sgl) :: plegendre ! function for calculation of Legendre polynomial
  real(sgl) :: pl(0:200) !
!
! ************************ Legendre polynomial *************************
!
! pl0, pl1    : help variables
!
  pl0 = 1.
  pl1 = x
  pl(0) = pl0
  pl(1) = pl1
  do i = 2, l
     pl(i) = (x * (2 * i - 1) * pl1 - (i - 1) * pl0) / i
     pl0 = pl1
     pl1 = pl(i)
  enddo
  plegendre = pl(l)
  return
end function plegendre
