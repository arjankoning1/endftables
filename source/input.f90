subroutine input
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read keywords
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
  character*1        :: ch
  character*80       :: word(40),value
  character*80       :: key
  character*80       :: infile
  character*80       :: line
  integer            :: i
  integer            :: ix
  integer            :: j
  integer            :: k
  integer            :: n
!
! Default values for options
!
  flagangle=.true.
  flagspectra=.false.
  flagddx=.false.
  lib=' '
  call getarg(1,infile)
  i=index(infile,'.')
  j=index(infile,'.pendf')
  k=index(infile,'.gendf')
  n=max(j,k)
  if (n.gt.0) then
    lib=trim(infile(i+1:n-1))
  else
    lib=trim(infile(i+1:80))
  endif
  source = 'ENDF'
  user = 'Arjan Koning'
  oformat = 'YANDF-0.1'
  open (unit=5,file=infile)
!
! Read input
!
  do i=1,Ninlines
    line = inline(i)
    call getkeywords(line,word)
    key=word(1)
    value=word(2)
    ch=word(2)(1:1)
    if (key.eq.'angle') then
      if (ch.eq.'n') flagangle=.false.
      if (ch.eq.'y') flagangle=.true.
      if (ch.ne.'y'.and.ch.ne.'n') goto 300
      cycle
    endif
    if (key.eq.'ddx') then
      if (ch.eq.'n') flagddx=.false.
      if (ch.eq.'y') flagddx=.true.
      if (ch.ne.'y'.and.ch.ne.'n') goto 300
      cycle
    endif
    if (key.eq.'spectra') then
      if (ch.eq.'n') flagspectra=.false.
      if (ch.eq.'y') flagspectra=.true.
      if (ch.ne.'y'.and.ch.ne.'n') goto 300
      cycle
    endif
    if (key.eq.'lib') then
      lib=value
      cycle
    endif
    if (key == 'source') then
      ix=index(line,'source')+7
      source=trim(adjustl(line(ix:80)))
      cycle
    endif
    if (key == 'user') then
      ix=index(line,'user')+5
      user=trim(adjustl(line(ix:80)))
      cycle
    endif
    if (key == 'format') then
      ix=index(line,'format')+7
      oformat=trim(adjustl(line(ix:80)))
      cycle
    endif
  enddo
  return
300 write(*,'(" ENDFTABLES-error: Wrong input: ",a80)') line
  stop
end subroutine input
