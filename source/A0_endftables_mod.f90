module endftables_mod
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: All global variables
!
! Revision    Date      Author           Description
! ====================================================
!    1     2023-12-29   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
  implicit none
  integer, parameter :: sgl = selected_real_kind(6,37)       ! single precision kind
  integer, parameter :: dbl = selected_real_kind(15,307)     ! double precision kind
  integer, parameter :: numinlines=100                       !
  integer, parameter :: nummf=40                             !
  integer, parameter :: nummt=1000                           !
  integer, parameter :: numline=2000000                     !
  integer, parameter :: numpar=6                              !
  integer, parameter :: numen=500000                         !
  integer, parameter :: numleg=100                            !
  integer, parameter :: numang=181                            !
  integer, parameter :: numiso=4                              !
  integer, parameter :: numsec=800                            !
  integer, parameter :: numen6=12000                        !
  integer, parameter :: numcov=2000                         !
  integer, parameter :: nummt0=207                          !
  integer, parameter :: numspec=200                          !
  integer, parameter :: numenspec=3000                         !
  integer, parameter :: numZ=124                          !
  integer, parameter :: numA=414                          !
!
! endftables
!
  character(len=80)  :: MTline(numline)                      !
!
! machine
!
  character(len=10)  :: date      ! date
!
! readinput 
!
  logical            :: flagangle
  logical            :: flagspectra
  logical            :: flagddx
  character*80       :: inline(numinlines)
  integer            :: Ninlines                           !
!
! readendf
!
  integer            :: Nendflines                           !
  character(len=80)  :: endfline(numline)                    !
  logical            :: Rmfmtexist(nummf, nummt)              !
  logical            :: mfmtexist(nummf, nummt)              !
  logical            :: mfexist(nummf)                       !
  logical            :: mtexist(nummt)                       !
  integer            :: sectionbeg(nummf, nummt)             !
  integer            :: sectionend(nummf, nummt)             !
!
! initial
!
  character*1        :: parsym(0:numpar)
  character*1        :: isochar(-1:numiso)
  character*2        :: nuc(numZ)
  character*10       :: MTreac(nummt,-1:numiso)                        !
  character(len=132) :: source      ! source of data
  character(len=132) :: oformat     ! format of data
  character(len=132) :: user        ! user of data
  real               :: pi
  real               :: deg2rad
  real(sgl)          :: leg(0:numleg,0:numang)         !
  real(sgl)          :: Eprod(0:numpar,numen)         !
  real(sgl)          :: xsprod(0:numpar,numen)         !
  integer            :: parZ(0:numpar)                        !
  integer            :: parN(0:numpar)                        !
  integer            :: parA(0:numpar)                        !
  integer            :: MTcode(nummt)                        !
  integer            :: Zix(nummt)                        !
  integer            :: Nix(nummt)                        !
  integer            :: pprod(nummt,0:numpar)                    !
!
! reactioncodes
!
  character(len=16)  :: reactionstring(nummt)
  integer            :: MTchannel(nummt)   
!
! MTinitial
!
  character(len=80)  :: endfhead                             !
  character(len=80)  :: endfMT                               !
!
! readMF1
!
  character*1        :: proj
  character*1        :: LISOchar
  character*2        :: El
  character*6        :: targetnuclide
  character*10       :: library
  character*10       :: EDATE
  character*80       :: lib
  character*33       :: author
  integer            :: Lis                                 !
  integer            :: Liso                                 !
  integer            :: Ztarget
  integer            :: Atarget
  integer            :: Ntarget
  integer            :: ZCN
  integer            :: ACN
  integer            :: NCN
  integer            :: Nnubar(3)                                  !
  integer            :: Nlambda                                 !
  integer            :: year
  real(sgl)          :: EMAX
  real(sgl)          :: Enubar(3,numen)        !
  real(sgl)          :: nubar(3,numen)        !
  real(sgl)          :: Cnubar(numen)
  real(sgl)          :: lambda(numen)
  real(sgl)          :: nuspont
  real(sgl)          :: EFR
  real(sgl)          :: DEFR
  real(sgl)          :: ENP
  real(sgl)          :: DENP
  real(sgl)          :: END1
  real(sgl)          :: DEND
  real(sgl)          :: EGP
  real(sgl)          :: DEGP
  real(sgl)          :: EGD
  real(sgl)          :: DEGD
  real(sgl)          :: EBDEL
  real(sgl)          :: DEBDEL
  real(sgl)          :: ENU
  real(sgl)          :: DENU
  real(sgl)          :: ERN
  real(sgl)          :: DERN
  real(sgl)          :: ET
  real(sgl)          :: DET
!
! readMF3
!
  integer            :: Nxs3(-1:numiso)                       !
  integer            :: Liss(-1:numiso)                       !
  real(sgl)          :: E3(-1:numiso,numen)         !
  real(sgl)          :: Q3(-1:numiso)         !
  real(sgl)          :: xs3(-1:numiso,numen)        !
  integer            :: Niso                                 !
!
! processMF3
!
  integer            :: Nxs(-1:numiso)                       !
  integer            :: Nprod(0:numpar)                      !
  real(sgl)          :: QI(-1:numiso)         !
  real(sgl)          :: E(-1:numiso,numen)         !
  real(sgl)          :: xs(-1:numiso,numen)        !
!
! readMF4
!
  integer            :: NEl                                  !
  integer            :: NEh                                  !
  integer            :: Nleg(numen6)                          !
  integer            :: NAh(numen6)                          !
  real(sgl)          :: E4(numen6)         !
  real(sgl)          :: E4l(numen6)         !
  real(sgl)          :: cleg(numen6,0:numleg)         !
  real(sgl)          :: E4h(numen6)         !
  real(sgl)          :: mu4(numen6,0:numang)         !
  real(sgl)          :: ang4(numen6,0:numang)         !
!
! processMF4
!
  integer            :: NE                                   !
  real(sgl)          :: angdis(numen6,0:numang)         !
!
! readMF6
!
  integer            :: NK
  integer            :: LIP(nummt,numsec)                          !
  integer            :: NY6(numsec)                          !
  integer            :: Nb6(numsec,numen6)         !
  integer            :: rpix(numsec,20)         !
  integer            :: MTix(nummt,-1:numiso)         !
  integer            :: Nrpix(numsec)
  real(sgl)          :: ZAP(numsec)         !
  real(sgl)          :: AWP(numsec)         !
  real(sgl)          :: EY6(numsec,numen6)         !
  real(sgl)          :: E6(numsec,numen6)         !
  real(sgl)          :: Y6(numsec,numen6)         !
  real(sgl),allocatable :: b6(:,:,:)         !
!
! readMF33
!
  integer            :: NEcov(-1:numiso)
  real(sgl)          :: Ecov(-1:numiso,0:numcov)               !
  real(sgl)          :: ycov(-1:numiso,numcov*numcov)               !
!
! processMF6
!
  integer            :: Zres(numsec)
  integer            :: Ares(numsec)
  integer            :: Nres(numsec)
  integer            :: Nrp(numsec)                          !
  integer            :: Nisom(nummt)                          !
  real(sgl)          :: Erp(numsec,numen6)         !
  real(sgl)          :: xsrp(numsec,numen6)         !
!
! integral   
!
  character(len=80)  :: spectrum(numspec)
  integer            :: Nspectrum
  integer            :: Nenspectrum(numspec)
  real(sgl)          :: Espectrum(numspec,numenspec)
  real(sgl)          :: fspectrum(numspec,numenspec)
!
! processMF6
!
  integer            :: NKres
  integer            :: Nisorp(0:numZ,0:numA)                          !
!
! processrp
!
  integer            :: Nresi
  real               :: Eres(0:numen)
  real               :: xsres(0:numen)
!
! processMF33
!
  logical            :: covexist
  real(sgl)          :: xslow(-1:numiso,numen)        !
  real(sgl)          :: xsupp(-1:numiso,numen)        !
end module
