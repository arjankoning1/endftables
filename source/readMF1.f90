subroutine readMF1(line,Nlines,MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read MF1
!
! Revision    Date      Author           Description
! ====================================================
!    1     2023-08-08   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use endftables_mod
!
! *** Declaration of local data
!
  implicit none
  character*3        :: massstring
  character*10       :: libf
  character*80       :: line(Nlines)
  character*80       :: block(Nlines)
  integer            :: Nlines
  integer            :: k0
  integer            :: LNU
  integer            :: nutype
  integer            :: MT
  integer            :: Nc
  integer            :: n
  integer            :: nlin
  integer            :: NR
  integer            :: NP
  integer            :: L
  integer            :: j
  integer            :: yy
  real               :: ZA
  real               :: AWI
  real               :: x(3*Nlines)
  real               :: y(3*Nlines)
  real               :: eps
!
! Read MF1
!
  if (MT == 451) then
    read(line(1)(1:11), '(es11.6)') ZA
    read(line(2)(23:44), '(2i11)') LIS,LISO
    read(line(3)(1:11), '(es11.6)') AWI
    read(line(3)(12:22), '(es11.6)') EMAX
    read(line(5)(23:32), '(a10)') EDATE
    read(line(5)(34:66), '(a33)') author
    read(line(7)(5:14), '(a10)') libf
    read(EDATE(9:10),'(i2)') yy
    if (yy >= 50 ) then
      year=1900+yy
    else
      year=2000+yy
    endif
    Ztarget=int(ZA)/1000
    Atarget=mod(int(ZA),1000)
    Ntarget=Atarget-Ztarget
    El=nuc(Ztarget)
    if (LISO > 0) then
      LISOchar=isochar(LISO)
    else
      LISOchar=' '
    endif
    massstring='   '
    write(massstring,'(i3)') Atarget
    targetnuclide = trim(El)// trim(adjustl(massstring)) // trim(Lisochar)
    eps=1.e-4
    k0=1
    if (abs(AWI).le.eps) k0=0
    if (abs(AWI-0.998623).le.eps) k0=2
    if (abs(AWI-0.999142).le.eps) k0=2
    if (abs(AWI-0.999170).le.eps) k0=2
    if (abs(AWI-1.996256).le.eps) k0=3
    if (abs(AWI-2.989596).le.eps) k0=4
    if (abs(AWI-2.989033).le.eps) k0=5
    if (abs(AWI-3.967131).le.eps) k0=6
    if (abs(AWI-3.968220).le.eps) k0=6
    proj = parsym(k0)
    ZCN = Ztarget + parZ(k0)
    NCN = Ntarget + parN(k0)
    ACN = Atarget + parA(k0)
    if (lib(1:1).eq.' ') then
      lib='evl'
      if (libf(1:6) == 'ENDF/B') lib='endfb8.0'
      if (libf(1:7) == 'JENDL-4') lib='jendl4.0'
      if (libf(1:8) == 'JENDL-HE') lib='jendl.he'
      if (libf(1:7) == 'CENDL-3') lib='cendl3.1'
      if (libf(1:5) == 'KAERI') lib='kaeri.g1'
      if (libf(1:3) == 'JEF') lib='jeff3.3'
      if (libf(1:8) == 'EAF-2010') lib='eaf.2010'
      if (libf(1:4) == 'IRDF') lib='irdff2.0'
      if (libf(1:3) == 'FEI') lib='irdff2.0'
      if (libf(1:5) == 'TENDL') lib='tendl.2023'
    endif
    EMAX=EMAX*1.e-6
  endif
  library=lib(1:10)
  n=0
  if (MT >= 452 .and. MT <= 456) then
    if (MT == 452) nutype=1
    if (MT == 455) nutype=2
    if (MT == 456) nutype=3
    read(line(1)(34:44), '(i11)') LNU
    if (LNU == 1) then
      if (MT <= 455) then
        read(line(2)(45:55), '(i11)') Nc
        n=2
        nlin = 1 + (Nc - 1) / 6
        n=n+nlin
        block(1:nlin) = line(n:n+nlin-1)
        call yblock(block,nlin,y)
        if (MT == 452) then
          do L = 1, Nc
            Cnubar(L) = y(L)
          enddo
          Nnubar(nutype)=Nc
        else
          do L = 1, Nc
            lambda(L) = y(L)
          enddo
          Nlambda=Nc
        endif
      endif
      if (MT >= 455) then
        n=n+2
        read(line(n)(1:11), '(es11.6)') nuspont
      endif
    endif
    if (LNU == 2) then
      n=2
      if (MT == 455) then
        read(line(2)(45:55), '(i11)') Nc
        nlin = 1 + (Nc - 1) / 6
        n=n+1+nlin
        block(1:nlin) = line(n:n+nlin-1)
        call yblock(block,nlin,y)
        do L = 1, Nc
          lambda(L) = y(L)
        enddo
        Nlambda=Nc
      endif
      read(line(n)(45:55), '(i11)') NR
      read(line(n)(56:66), '(i11)') NP
      nlin = 1 + (NR - 1) / 3
      n=n+1+nlin
      nlin = 1 + (NP - 1) / 3
      block(1:nlin) = line(n:n+nlin-1)
      call xyblock(block,nlin,x,y)
      do j = 1, NP
        Enubar(nutype,j) = x(j) * 1.e-6
        nubar(nutype,j) = y(j)
      enddo
      Nnubar(nutype)=NP
    endif
  endif
  if (MT == 458) then
    read(line(3)(1:66), '(6es11.6)') EFR,DEFR,ENP,DENP,END1,DEND
    read(line(4)(1:66), '(6es11.6)') EGP,DEGP,EGD,DEGD,EBDEL,DEBDEL
    read(line(5)(1:66), '(6es11.6)') ENU,DENU,ERN,DERN,ET,DET
  endif
return
end subroutine readMF1
