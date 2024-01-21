subroutine checkkeyword
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Check keywords keywords
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
  character*80       :: word(40)
  character*80       :: key
  integer, parameter :: numkey=5
  character*80       :: keyword(numkey)
  integer            :: i
  integer            :: j
!
! Keywords
!
  data (keyword(i),i=1,numkey) / ' ', 'angle', 'ddx', 'lib', 'spectra'/
!
! Check
!
  A: do i=1,Ninlines
    call getkeywords(inline(i),word)
    key=word(1)
    if (key(1:1).eq.'#') cycle
    do j=1,numkey
      if (keyword(j).eq.key) cycle A
    enddo
    write(*,'(/" ENDFTABLES-error: Wrong keyword: ",a20)') key
    stop
  enddo A
  return
  stop
end subroutine checkkeyword
