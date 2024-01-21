subroutine processrp(k)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process residual cross sections
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
  logical            :: lexist
  character(len=3)   :: MTstring
  character(len=80)  :: endfMT0
  character(len=132) :: endffile
  character(len=132) :: line
  character(len=132) :: key
  integer, parameter :: NMT = 20
  integer            :: istat
  integer            :: k
  integer            :: keyix
  integer            :: L
  integer            :: n
  integer            :: kk
  integer            :: MT
  integer            :: nen
  integer            :: Nxsmt(NMT)
  integer            :: i
  integer            :: j
  integer            :: kmin
  real               :: Em(0:numen,NMT)
  real               :: xsm(0:numen,NMT)
  real               :: Emm(0:numen)
  real               :: xsmm(0:numen)
  real               :: Emin
  real               :: EE
  real               :: Ea
  real               :: Eb
  real               :: Ya
  real               :: Yb
  real               :: Y
  real               :: xx(numen)
  real               :: yy(numen)
!
! Residual production cross sections including low energy MT cross sections
!
  Nresi=0
  Eres=0.
  xsres=0.
  Em=0.
  xsm=0.
  Nxsmt=0
  L = LIP(5,k)
  N = 0
  if (Nrpix(k) >= 1) then
    kmin = 1
    Emin = 1000.
    do kk = 1, Nrpix(k)
      MT=rpix(k,kk)
      MTstring='000'
      write(MTstring(1:3), '(i3.3)') MT
      endfMT0=trim(endfhead)//'-MT'//MTstring
      if (L == -1) then
        endffile = trim(endfMT0)//'.'//trim(lib)
      else
        endffile = trim(endfMT0)//isochar(L)//'.'//trim(lib)
      endif
      inquire (file=endffile,exist=lexist)
      if (lexist) then
        open (unit = 1, status = 'unknown', file = endffile)
        do
          read(1,'(a)',iostat = istat) line
          if (istat == -1) exit
          key='entries'
          keyix=index(line,trim(key))
          if (keyix > 0) then
            read(line(keyix+len_trim(key)+2:80),*, iostat = istat) Nxsmt(kk)
            read(1,'(/)')
            do nen = 1, Nxsmt(kk)
              read(1,*) Em(nen,kk),xsm(nen,kk)
            enddo
            exit
          endif
        enddo
        close (1)
      endif
      Em(0,kk)=0.
      xsm(0,kk)=xsm(1,kk)
      if (Em(1,kk) < Emin) then
        Emin = Em(1,kk)
        kmin = kk
      endif
    enddo
    N = Nxsmt(kmin)
    do nen = 1, N
      Eres(nen) = Em(nen,kmin)
      xsres(nen) = xsm(nen,kmin)
    enddo
    do kk = 1, Nrpix(k)
      if (kk /= kmin .and. Nxsmt(kk) > 0) then
        Emm = 0.
        xsmm = 0.
        do nen = 0, Nxsmt(kk)
          Emm(nen) = Em(nen,kk)
          xsmm(nen) = xsm(nen,kk)
        enddo
        do nen = 1, N
          EE = Eres(nen)
          if (EE == Eres(nen+1)) EE =max(EE - 1.e-5, 1.e-11)
          call locate(Em, 0, Nxsmt(kk), EE, j)
          Ea = Emm(j)
          Eb = Emm(j+1)
          Ya = xsmm(j)
          Yb = xsmm(j+1)
          if (Eb-Ea < 1.e-6) then
            Y = Ya
          else
            call pol1(Ea, Eb, Ya, Yb, EE, Y)
          endif
          xsres(nen) = xsres(nen) + Y
        enddo
      endif
    enddo
  endif
  do nen = 1, NY6(k)
    if (N > 0) then
      if (Erp(k,nen) <= Eres(N)) cycle
    endif
    if (xsrp(k,nen) > 0.) then
      if ( N < numen) then
        N =  N + 1
        Eres(N) = Erp(k,nen)
        xsres(N) = xsrp(k,nen)
      endif
    endif
  enddo
  Nresi=N
!
! Cleanup table
!
  N = Nresi
  do i=1,N
    xx(i)=Eres(i)
    yy(i)=xsres(i)
  enddo
  call cleantable(xx,yy,N)
  Nresi = N
  do i=1,N
    Eres(i)=xx(i)
    xsres(i)=yy(i)
  enddo
  return
end subroutine processrp
