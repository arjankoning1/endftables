subroutine getkeywords(line, word)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Get keywords and other words from input lines
!
! Revision    Date      Author           Description
! ====================================================
!    1     05-04-2016   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!  
  implicit none
  character*1  chprev,ch
  character*80 line,word(40)
  integer      i,nkey,ibeg,iend
!
! *** Get words from input line
!  
! From each input line we retrieve the keyword and strings with values. These are all stored in the array 'word'.
!  
  do i=1,40
    word(i)='                                                  '
  enddo
  chprev=' '
  nkey=0
  ibeg=1
  do i=1,80
    if (i.gt.1) chprev=line(i-1:i-1)
    ch=line(i:i)
    if (ch.ne.' '.and.chprev.eq.' ') then
      nkey=nkey+1
      ibeg=i
    endif
    if (ch.eq.' '.and.chprev.ne.' ') then
      iend=i-1
      word(nkey)=line(ibeg:iend)
    endif
  enddo
  return
end subroutine getkeywords
