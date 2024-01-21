subroutine yblock(line,N,y)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read block of y data
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
  implicit none
  integer            :: N
  character*80       :: line(N)
  real               :: y(6*N)
  integer            :: i
  integer            :: k
  integer            :: j
  y = 0.
  do i = 1, N
    k = 6 * (i - 1)
    read(line(i), '(6e11.6)') (y(k+j), j = 1, 6)
  enddo
  return
end subroutine yblock
