subroutine writeMT(line,Nlines,MF,MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write MT section
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
  character*80       :: line(Nlines)
  character*80       :: endfMFMT
  character*132      :: endffile
  character*3        :: MTstring
  character*2        :: MFstring
  integer            :: Nlines
  integer            :: MF
  integer            :: MT
  integer            :: i
!
! Write MF/MT file
!
  MFstring='00'
  write(MFstring(1:2), '(i2.2)') MF
  MTstring='000'
  write(MTstring(1:3), '(i3.3)') MT
  endfMFMT=trim(endfhead)//'-MF'//MFstring//'-MT'//MTstring
  endffile = trim(endfMFMT)//'.'//trim(lib)
  open (unit = 1, status = 'unknown', file = endffile)
  do i = 1, Nlines
    write(1, '(a80)') line(i)
  enddo
  close (1)
  return
end subroutine writeMT
