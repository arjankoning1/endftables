subroutine processMF6(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process MF6
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
  integer            :: iang
  integer            :: Z
  integer            :: A
  integer            :: nen
  integer            :: j
  integer            :: k
  integer            :: n
  integer            :: kk
  integer            :: k2
  integer            :: kb
  integer            :: iso
  integer            :: kiso(0:numiso)
  integer            :: i
  integer            :: imt
  integer            :: nen2
  integer            :: L
  integer            :: Na
  real               :: twopi
  real               :: EE6(0:numen)
  real               :: xs6(0:numen)
  real               :: Eiso(0:numen)
  real               :: xsiso(0:numen)
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
  real               :: ma
  real               :: mb
  real               :: Aa
  real               :: Ab
  real               :: Aang
!
! *** Process yields
!
  Ee6 = 0.
  xs6 = 0.
  do nen = 1, Nxs3(-1)
    Ee6(nen) = E3(-1,nen)
    xs6(nen) = xs3(-1,nen)
  enddo
  Ee6(0) = 0.
  xs6(0) = xs6(1)
!
! Record residual products
!
  if (MT == 5) then
    Nisorp=0
    NKres=NK
    do k = 1, NKres
      Z=int(ZAP(k))/1000
      A=mod(int(ZAP(k)),1000)
      Z=min(Z,numZ)
      A=min(A,numA)
      Nisorp(Z,A)=max(Nisorp(Z,A),LIP(MT,k))
      Zres(k)=Z
      Ares(k)=A
      Nres(k)=A-Z
    enddo
!
! Check if there are isomers
!
    do k = 1, NKres
      Z=Zres(k)
      A=Ares(k)
      if (Nisorp(Z,A) == 0) LIP(MT,k) = -1
    enddo
!
! Produce residual production cross sections with MF3-MF6 combination
!
    do k = 1, NKres
      n=0
      do nen = 1, NY6(k)
        n = n + 1
        EE = EY6(k,nen)
        if (EE == EY6(k,nen+1)) EE = max(EE - 1.e-5, 1.e-11)
        Erp(k,nen) = EY6(k,nen)
        call locate(Ee6, 0, Nxs3(-1), EE, j)
        Ea = Ee6(j)
        Eb = Ee6(j+1)
        Ya = xs6(j)
        Yb = xs6(j+1)
        call pol1(Ea, Eb, Ya, Yb, Ee, Y)
        xsrp(k,n) = Y6(k,nen) * Y
      enddo
      Nrp(k) = n
    enddo
!
! Reproduce total residual production cross sections out of isomeric
!
    k2 = NKres
    do k = 1, NKres
      Z=Zres(k)
      A=Ares(k)
      N=A-Z
      kiso=0
      if (LIP(MT,k) == 0) then
        i=0
        kiso(i)=k
        do kk = k+1, k+Nisorp(Z,A)
          i=i+1
          kiso(i)=kk
        enddo
        k2 = k2 + 1
        Zres(k2)=Z
        Nres(k2)=N
        Ares(k2)=A
        LIP(MT,k2) = -1
        NY6(k2) = NY6(k)
        Nrp(k2) = Nrp(k)
        do nen = 1, NY6(k)
          EY6(k2,nen) = EY6(k,nen)
          Erp(k2,nen) = EY6(k,nen)
          xsrp(k2,nen) = xsrp(k,nen)
          EE = EY6(k2,nen)
          if (EE == EY6(k2,nen+1)) EE = max(EE - 1.e-5, 1.e-11)
          do kk = 1, Nisorp(Z,A)
            kb = kiso(kk)
            do nen2 = 1, NY6(kb)
              Eiso(nen2) = EY6(kb,nen2)
              call locate(Ee6, 0, Nxs3(-1), Eiso(nen2), j)
              Ea = Ee6(j)
              Eb = Ee6(j+1)
              Ya = xs6(j)
              Yb = xs6(j+1)
              call pol1(Ea, Eb, Ya, Yb, Ee, Y)
              xsiso(nen2) = Y6(kb,nen2) * Y
            enddo
            Eiso(0)=0.
            xsiso(0) = xsiso(1)
            call locate(Eiso, 0, NY6(kb), EE, j)
            Ea = Eiso(j)
            Eb = Eiso(j+1)
            Ya = xsiso(j)
            Yb = xsiso(j+1)
            call pol1(Ea, Eb, Ya, Yb, Ee, Y)
            xsrp(k2,nen) = xsrp(k2,nen) + Y
          enddo
        enddo
      endif
    enddo
    NKres = k2
!
! Determine MT numbers of total residual production cross sections
!
    do k = 1, NKres
      Z = ZCN - Zres(k)
      N = NCN - Nres(k)
      L = LIP(MT,k)
      kk = 0
      do iso = -1, numiso
        do imt = 4, nummt0
          if (MTcode(imt) == -1) cycle
          if (imt == 4 .or. (imt >= 16 .and. imt <= 49) .or. (imt >= 102 .and. imt <= 117)) then
            if (Z == Zix(imt) .and. N == Nix(imt) .and. iso == L) then
              kk = kk + 1
              rpix(k,kk) = imt
              if (imt == 4 .or. imt == 16 .or. imt == 17 .or. imt == 37 .or. imt == 102 .or. imt == 103) MTix(imt,iso) = k
              Nisom(imt) = max(Nisom(imt),iso)
              mfmtexist(3,imt)=.true.
              MTexist(imt)=.true.
            endif
          endif
        enddo
        Nrpix(k) = kk
      enddo
    enddo
  endif
!
! *** Process spectra and DDX
!
  if (flagspectra .or. flagddx) then
  endif
!
! *** Process angular distributions
!
  if (flagangle .and. MT == 2) then
    twopi=2.*pi
    do nen = 1, NE
      EE = E6(1,nen)
      call locate(Ee6, 0, Nxs3(-1), Ee, j)
      Ea = Ee6(j)
      Eb = Ee6(j+1)
      Ya = xs6(j)
      Yb = xs6(j+1)
      call pol1(Ea, Eb, Ya, Yb, Ee, Y)
      do iang=0,180
        m4(iang)=mu4(nen,iang)
        a4(iang)=ang4(nen,iang)
      enddo
      NA = Nah(nen-NEl)
      do iang=0,180
        angle=real(iang)*deg2rad
        mu=cos(angle)
        call locate(m4, 0, Nah(nen), mu, j)
        ma = m4(j)
        mb = m4(j+1)
        Aa = a4(j)
        ab = a4(j+1)
        call pol1(ma, mb, Aa, Ab, mu, Aang)
        angdis(nen,iang)=Y/twopi*Aang
      enddo
    enddo
  endif
  deallocate(b6)
  return
end subroutine processMF6
