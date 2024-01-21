subroutine xyblock(line,N,x,y)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read block of xy data
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
  implicit none
  character*80       :: line(N)
  integer            :: N
  integer            :: i
  integer            :: k
  integer            :: j
  real               :: x(3*N)
  real               :: y(3*N)
  x = 0.
  y = 0.
  do i = 1, N
    k = 3 * (i - 1)
    read(line(i), '(6e11.6)') (x(k+j), y(k+j), j = 1, 3)
  enddo
  return
end subroutine xyblock
