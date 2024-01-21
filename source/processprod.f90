subroutine processprod(k)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process production cross sections
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
  integer            :: nen
  integer            :: type
  integer            :: nen2
  integer            :: N
  integer            :: i
  integer            :: j
  real               :: Ep(0:numen)
  real               :: xsp(0:numen)
  real               :: EE
  real               :: Ea
  real               :: Eb
  real               :: Ya
  real               :: Yb
  real               :: Y
  real               :: xx(numen)
  real               :: yy(numen)
!
! 
!
  if (Zres(k) > 2 .or. Ares(k) > 4 ) return
  if (Zres(k) == 0 .and. Ares(k) == 0) type=0
  if (Zres(k) == 0 .and. Ares(k) == 1) type=1
  if (Zres(k) == 1 .and. Ares(k) == 1) type=2
  if (Zres(k) == 1 .and. Ares(k) == 2) type=3
  if (Zres(k) == 1 .and. Ares(k) == 3) type=4
  if (Zres(k) == 2 .and. Ares(k) == 3) type=5
  if (Zres(k) == 2 .and. Ares(k) == 4) type=6
  do nen=1,Nprod(type)
    EE=Eprod(type,nen)
    if (EE == Eprod(type,nen+1)) EE = max(EE - 1.e-5, 1.e-11)
    do nen2 = 1, NY6(k)
      Ep(nen2) = Erp(k, nen2)
      xsp(nen2) = xsrp(k, nen2)
    enddo
    Ep(0)=0.
    xsp(0) = xsp(1)
    if (EE >= Ep(0) .and. EE <= Ep(NY6(k))) then
      call locate(Ep, 0, NY6(k), EE, j)
      Ea = Ep(j)
      Eb = Ep(j+1)
      Ya = xsp(j)
      Yb = xsp(j+1)
      call pol1(Ea, Eb, Ya, Yb, EE, Y)
      xsprod(type,nen) = xsprod(type,nen) + Y
    endif
  enddo
!
! Cleanup table
!
  N = Nprod(type)
  do i=1,N
    xx(i)=Eprod(type,i)
    yy(i)=xsprod(type,i)
  enddo
  call cleantable(xx,yy,N)
  Nprod(type) = N
  do i=1,N
    Eprod(type,i)=xx(i)
    xsprod(type,i)=yy(i)
  enddo
  return
end subroutine processprod
