subroutine cleantable(x,y,NP)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Clean table from non-increasing and double energies
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use endftables_mod
!
! *** Declaration of local data
!
  implicit none
  logical            :: flagdel
  integer            :: NP
  integer            :: N
  integer            :: i
  integer            :: j
  real               :: x(NP)
  real               :: y(NP)
!
! Delete decreasing energies
!
  N = NP
100 flagdel=.false.
  do i = 2, N
    if (x(i) < x(i-1)) then
      do j=i,N-1
        x(j)=x(j+1)
        y(j)=y(j+1)
      enddo
      flagdel=.true.
      exit
    endif
  enddo
  if (flagdel) then
    N=N-1
    goto 100
  endif
!
! Delete multiple ( > 2) points with equal incident energy
!
200 flagdel=.false.
  do i = 1, N-2
    if (x(i) >= x(i+1) .and. x(i) == x(i+2)) then
      do j=i+1,N-1
        x(j)=x(j+1)
        y(j)=y(j+1)
      enddo
      flagdel=.true.
      exit
    endif
  enddo
  if (flagdel) then
    N=N-1
    goto 200
  endif
  if (N > 1) then
    if (x(N) == x(N-1)) N = N - 1
  endif
  NP = N
  return
end subroutine cleantable
