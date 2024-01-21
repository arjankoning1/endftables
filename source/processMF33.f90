subroutine processMF33
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process MF33
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
  integer            :: n
  integer            :: NE3
  integer            :: i
  integer            :: j
  integer            :: L
  integer            :: nen
  real               :: Ee
  real               :: Etmp(0:numcov)
  real(sgl)          :: relerr(numcov)               !
!
! *** Process MF33
!
  do L= -1, Niso
    if (NEcov(L) == 0) cycle
    relerr=0.
    NE3 = Nxs(L)
    n= 1 - NEcov(L)
    do j=1,NEcov(L)-1
      n=n+1+NEcov(L)-j
      relerr(j)=sqrt(ycov(L, n))
    enddo
    if (NEcov(L) > 1) relerr(NEcov(L))=relerr(NEcov(L)-1)
    covexist=.true.
    do nen = 1, NE3
      Ee = E(L,nen)
      j = NEcov(L)
      if (Ee <= Ecov(L, 0)) then
        j = 1
      else
        if (Ee >= Ecov(L, NEcov(L))) then
          j = NEcov(L)
        else
          do i= 0, NEcov(L)
            Etmp(i) = Ecov(L, i)
          enddo
          call locate(Etmp, 0, NEcov(L), Ee, j)
        endif
      endif
      j=max(j,1)
      xslow(L,nen) = max (xs(L,nen) * (1.-relerr(j)),0.)
      xsupp(L,nen) = xs(L,nen) * (1.+relerr(j))
    enddo
  enddo
  return
end subroutine processMF33
