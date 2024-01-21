subroutine residual
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Residual production cross sections
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
!
! Residual production cross sections 
!
  do k = 1, NKres
    call processrp(k)
    call writerp(k)
    if (Nspectrum > 0) call integralrp(k)
  enddo
  return
end subroutine residual
