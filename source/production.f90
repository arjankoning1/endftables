subroutine production
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Particle production cross sections
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
  integer            :: k
  integer            :: type
!
! Particle production cross sections 
!
  do k = 1, NKres
    call processprod(k)
  enddo
  do type = 0, 6 
    call writeprod(type)
  enddo
  return
end subroutine production
