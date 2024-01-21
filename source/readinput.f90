subroutine readinput
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read input file with options
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
  character*80       :: infile
  integer            :: i
  integer            :: istat
  integer            :: k
!
! Read input
!
  Ninlines=0
  infile='endftables.inp'
  inquire (file=infile,exist=lexist)
  if (.not.lexist) return
  open (unit = 1, status = 'unknown', file = infile)
  i = 1
  do
    read(1, '(a132)', iostat = istat) inline(i)
    if (istat ==  -1) exit
    if (i.gt.numinlines) then
      write(*,'(" ENDFTABLES-error: Number of input lines exceeds ",i5)') numinlines
      write(*,'(" numlines in A0_endftables_mod should be increased")')
      stop
    endif
    i = i + 1
  enddo
  Ninlines = i - 1
!
! ************** Convert uppercase to lowercase characters *************
!
  do i=1,Ninlines
    do k=1,80
      if (inline(i)(k:k).ge.'A'.and.inline(i)(k:k).le.'Z') inline(i)(k:k)=char(ichar(inline(i)(k:k))+32)
    enddo 
  enddo 
  return
end subroutine readinput
