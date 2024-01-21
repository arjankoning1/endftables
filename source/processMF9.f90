subroutine processMF9
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process MF9
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
  integer            :: L
  integer            :: k
  integer            :: nen
  real               :: E0(0:numen)
  real               :: xs0(0:numen)
  real               :: Ea
  real               :: Eb
  real               :: xsa
  real               :: xsb
  real               :: xsc
  real               :: E9
  real               :: Y9
!
! *** Process MF9: Interpolate
!
  E9 = 0.
  Y9 = 0.
  do k = 1, Nxs3(-1)
    E0(k) = E3(-1, k)
    xs0(k) = xs3(-1, k)
  enddo
  do L = 0, Niso
    do k = 1, Nxs3(L)
      E9 = E3(L, k)
      Y9 = xs3(L, k) * 0.001
      call locate(E0, 0, Nxs3(-1), E9, nen)
      Ea = E0(nen)
      Eb = E0(nen+1)
      xsa = xs0(nen)
      xsb = xs0(nen+1)
      call pol1(Ea, Eb, xsa, xsb, E9, xsc)
      xs3(L,k) = xsc * Y9
    enddo
  enddo
  return
end subroutine processMF9
