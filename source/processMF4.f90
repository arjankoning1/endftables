subroutine processMF4
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process MF4
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
  integer            :: iang
  integer            :: nen
  integer            :: j
  integer            :: L
  integer            :: Na
  real               :: twopi
  real               :: EE4(0:numen)
  real               :: xs4(0:numen)
  real               :: EE
  real               :: Ea
  real               :: Eb
  real               :: Ya
  real               :: Yb
  real               :: Y
  real               :: angle
  real               :: m4(0:numang)
  real               :: a4(0:numang)
  real               :: mu
  real               :: sum
  real               :: ma
  real               :: mb
  real               :: Aa
  real               :: Ab
  real               :: Aang
!
! *** Process MF4
!
  twopi=2.*pi
  do nen = 1, Nxs3(-1)
    Ee4(nen) = E3(-1,nen)
    xs4(nen) = xs3(-1,nen)
  enddo
  Ee4(0) = 0.
  xs4(0) = xs4(1)
  NE = NEl + NEh
  do nen = 1, min(NEl,numen6)
    E4(nen) = E4l(nen)
  enddo
  do nen = NEl + 1, min(NE,numen6)
    E4(nen) = E4h(nen-NEl)
  enddo
  do nen = 1, min(NE,numen6)
    Ee = E4(nen)
    Y = 0.
    if (Ee >= Ee4(0) .and. Ee <= Ee4(Nxs3(-1))) then
      call locate(Ee4, 0, Nxs3(-1), Ee, j)
      Ea = Ee4(j)
      Eb = Ee4(j+1)
      Ya = xs4(j)
      Yb = xs4(j+1)
      call pol1(Ea, Eb, Ya, Yb, Ee, Y)
    endif
    if (nen <= NEl) then
      do iang=0,180
        angle=real(iang)*deg2rad
        mu=cos(angle)
        sum=0.
        do L=0,Nleg(nen)
          sum=sum+0.5*(2.*L+1)*cleg(nen,L)*leg(L,iang)
        enddo
        angdis(nen,iang)=Y/twopi*sum
      enddo
    else
      NA = Nah(nen-NEl)
      do iang=1,NA  
        m4(iang)=mu4(nen,iang)
        a4(iang)=ang4(nen,iang)
      enddo
      m4(0)=1.
      a4(0)=a4(1)
      do iang=0,180
        angle=real(iang)*deg2rad
        mu=cos(angle)
        if (mu >= m4(0) .and. mu <= m4(NA)) then
          call locate(m4, 0, NA, mu, j)
          ma = m4(j)
          mb = m4(j+1)
          Aa = a4(j)
          Ab = a4(j+1)
          call pol1(ma, mb, Aa, Ab, mu, Aang)
          angdis(nen,iang)=Y/twopi*Aang
        endif
      enddo
    endif
  enddo
  return
end subroutine processMF4
