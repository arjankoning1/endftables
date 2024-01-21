subroutine pol1(x1, x2, y1, y2, x, y)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Interpolation of first order
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
  real :: x1  ! coordinates of intersection points inside the bin
  real :: x2  ! coordinates of the 2nd summit of the triangle
  real :: y1  ! coordinates of the 1st summit of the triangle
  real :: y2  ! coordinates of the 2nd summit of the triangle
  real :: fac !
  real :: x   ! help variable
  real :: y   ! coordinates of the point to test
!
! ***************************** Interpolation **************************
!
  fac = (x-x1)/(x2-x1)
  y = y1 + fac * (y2 - y1)
  return
end subroutine pol1
