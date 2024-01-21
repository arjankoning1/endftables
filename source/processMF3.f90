subroutine processMF3(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process MF3
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
  integer            :: MT
  integer            :: L
  integer            :: k
  integer            :: N
  integer            :: nen
  integer            :: i
  integer            :: j
  integer            :: type
  integer            :: nen2
  real               :: Eiso(0:numen)
  real               :: xsiso(0:numen)
  real               :: EE
  real               :: Ea
  real               :: Eb
  real               :: Ya
  real               :: Yb
  real               :: Y
  real               :: xx(numen)
  real               :: yy(numen)
  real               :: Ep(0:numen)
  real               :: xsp(0:numen)
!
! *** Process MF3: Add high-energy data
!
  Niso = max(Niso,Nisom(MT))
  E = E3 
  QI = Q3 * 1.e-6
  xs = xs3 
  Nxs = Nxs3 
  do L = -1, Niso
    k = MTix(MT,L)
    nen = 0
    do i= Nxs3(L), 1, -1
      if (xs3(L,i) > 0.) then
        nen = i
        exit
      endif
    enddo
    Nxs3(L) = nen
    if (k > 0) then
      do i = 1, Nrp(k)
        if (xsrp(k,i) > 0.) then
          nen = nen + 1
          E(L,nen) = Erp(k, i)
          xs(L,nen) = xsrp(k, i)
        endif
      enddo
    endif
    Nxs(L) = nen
  enddo
!
! Create total cross sections if only ground state and isomeric exist (EAF)
!
  if (Nxs(-1) == 0 .and. Niso > 0) then
    Nxs(-1)=Nxs(0)
    do nen = 1, Nxs(0)
      E(-1,nen) = E(0, nen)
      xs(-1,nen) = xs(0, nen)
      EE = E(-1,nen)
      if (nen > 1) then
        if (EE == E(-1, nen-1)) EE = EE + 1.e-5
      endif
      do L = 1, Niso
        do nen2 = 1, Nxs(L)
          Eiso(nen2) = E(L, nen2)
          xsiso(nen2) = xs(L, nen2)
        enddo
        Eiso(0)=0.
        xsiso(0) = xsiso(1)
        call locate(Eiso, 0, Nxs(L), EE, j)
        Ea = Eiso(j)
        Eb = Eiso(j+1)
        Ya = xsiso(j)
        Yb = xsiso(j+1)
        call pol1(Ea, Eb, Ya, Yb, EE, Y)
        xs(-1,nen) = xs(-1,nen) + Y
      enddo
    enddo
  endif
!
! Cleanup table
!
  do L = -1, Niso
    N = Nxs(L)
    do i=1,N
      xx(i)=E(L,i)
      yy(i)=xs(L,i)
    enddo
    call cleantable(xx,yy,N)
    Nxs(L) = N
    do i=1,N
      E(L,i)=xx(i)
      xs(L,i)=yy(i)
    enddo
  enddo
!
! Create total particle production cross sections
!
  if (MT <= 4) then
    Eprod=0.
    do type=0,6
      Nprod(type)=Nxs(-1)
      do nen=1,Nxs(-1)
        Eprod(type,nen)=E(-1,nen)
      enddo
    enddo
  endif
  if (.not.Rmfmtexist(3,MT)) return
  if (MT >= 4) then
    do nen2 = 1, Nxs(-1)
      Ep(nen2) = E(-1, nen2)
      xsp(nen2) = xs(-1, nen2)
    enddo
    do nen=1,Nprod(1)
      EE=Eprod(1,nen)
      if (nen > 1) then
        if (EE == Eprod(1,nen-1)) EE = EE + 1.e-5
      endif
      Ep(0)=0.
      xsp(0) = xsp(1)
      if (EE >= Ep(0) .and. EE <= Ep(Nxs3(-1))) then
        call locate(Ep, 0, Nxs3(-1), EE, j)
        Ea = Ep(j)
        Eb = Ep(j+1)
        Ya = xsp(j)
        Yb = xsp(j+1)
        call pol1(Ea, Eb, Ya, Yb, EE, Y)
        do type=0,6
          xsprod(type,nen) = xsprod(type,nen) + Y*pprod(MT,type)
        enddo
      endif
    enddo
  endif
  return
end subroutine processMF3
