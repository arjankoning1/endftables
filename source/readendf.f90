subroutine readendf
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read ENDF-6 file 
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
  integer            :: i                                    !
  integer            :: istat                                !
  integer            :: MF                                   ! MF-number
  integer            :: MT                                   ! MT-number
  integer            :: MTprev
!
! Read ENDF-6 file 
!
  i=0
  do 
    i=i+1
    if (i > numline) then
      write(*,'(" ENDFTABLES-error: Number of ENDF lines exceeds ",i7)') numline
      stop
    endif
    read(5, '(a80)', iostat = istat) endfline(i)
    if (istat ==  -1) exit
  enddo
  Nendflines=i-1
!
! Determine when MF/MT sections are
!
  Rmfmtexist=.false.
  mfmtexist=.false.
  mfexist=.false.
  mtexist=.false.
  sectionbeg=0
  sectionend=0
  MTprev=0
  do i=2,Nendflines
    read(endfline(i)(71:72), '(i2)') MF
    read(endfline(i)(73:75), '(i3)') MT
    if (MT /= 0 .and. MTprev ==0) then
      sectionbeg(MF,MT)=i
      Rmfmtexist(MF,MT)=.true.
      mfmtexist(MF,MT)=.true.
      mfexist(MF)=.true.
      mtexist(MT)=.true.
    endif
    if (MT == 0 .and. MTprev /=0) sectionend(MF,MTprev)=i
    MTprev=MT
  enddo
end subroutine readendf
