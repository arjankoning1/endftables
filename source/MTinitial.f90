subroutine MTinitial(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialize data per MT number
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
  character*3            :: mass 
  character*3            :: MTstring 
  integer                :: MT
  integer                :: is
!
! *** Initialization
!
  oformat='YANDF-0.1'
  mass='000'
  write(mass(1:3),'(i3.3)') Atarget
  endfhead = proj//'-'//trim(El)//mass
  if (LISO > 0) endfhead = trim(endfhead)//LISOchar
  MTstring='000'
  write(MTstring(1:3), '(i3.3)') MT
  endfMT=trim(endfhead)//'-MT'//MTstring
  if (MT == 451) return
  if (MT == 151) return
  do is=-1,numiso
    MTreac(MT,is)(2:2) = proj
  enddo
!
! Cross sections and yields
!
  E=0.
  xs=0.
  Nxs=0
  E3=0.
  Q3=0.
  xs3=0.
  Nxs3=0
  Niso=-1
  NK=0
  Nb6=0
  ZAP=0.
  AWP=0.
  E6=0.
  Y6=0.
!
! Fission neutrons
!
  Enubar=0.
  nubar=0.
  Nnubar=0
  Cnubar=0.
  lambda=0.
  Nlambda=0
  nuspont=0.
!
! Angular distributions
!
  E4=0.
  NE=0
  E4l=0.
  E4h=0.
  NEl=0
  NEh=0
  Nleg=0
  NAh=0
  cleg=0.
  angdis=0.
  mu4=0.
  ang4=0.
!
! Covariances
!
  NEcov=0
  Ecov=0
  ycov=0
  covexist=.false.
  xslow=0.
  xsupp=0.
  return
end subroutine MTinitial
