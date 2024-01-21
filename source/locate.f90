subroutine locate(xx, ib, ie, x, j)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Find value in ordered table
!
! Revision    Date      Author           Description
! ====================================================
!    1     2016-03-04   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
  implicit none
  logical   :: ascend   !
  integer   :: ib       !
  integer   :: ie       !
  integer   :: j        !
  integer   :: jl       !
  integer   :: jm       !
  integer   :: ju       !
  real      :: x        ! help variable
  real      :: xx(0:ie) !
!
! ******************************* Search *******************************
!
! Find j such that xx(j) <= x < xx(j+1) or xx(j) > x >= xx(j+1)
!
  j = 0
  if (ib > ie) return
  jl = ib - 1
  ju = ie + 1
  ascend = xx(ie) >= xx(ib)
  10  if (ju - jl > 1) then
    jm = (ju + jl) / 2
    if (ascend.eqv.(x >= xx(jm))) then
      jl = jm
    else
      ju = jm
    endif
    goto 10
  endif
  if (x == xx(ib)) then
    j = ib
  else if (x == xx(ie)) then
    j = ie - 1
  else
    j = jl
  endif
  return
end subroutine locate
