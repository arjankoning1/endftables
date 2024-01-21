subroutine readMF3(line,Nlines,MF,MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read MF3, MF9 and MF10
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
  character*80       :: block(Nlines)
  integer            :: Nlines
  integer            :: MF
  integer            :: MT
  integer            :: Lbeg
  integer            :: NSt
  integer            :: n
  integer            :: j
  integer            :: NR
  integer            :: NP
  integer            :: nlin
  integer            :: L
  integer            :: LL
  real               :: x(3*Nlines)
  real               :: y(3*Nlines)
!
! Read MF3, MF9 and MF10
!
  if (MF == 3.or.MT.eq.18) then
    Lbeg=-1
    NSt=0
  else
    Lbeg=0
    n=1
    read(line(n)(45:55), '(i11)') NSt
  endif
  Niso=NSt-1
  n=2
  do L=Lbeg,Niso
    read(line(n)(12:22), '(e11.6)') Q3(L)
    read(line(n)(34:44), '(i11)') LL
    read(line(n)(45:55), '(i11)') NR
    read(line(n)(56:66), '(i11)') NP
    nlin = 1 + (NR - 1) / 3
    n=n+1+nlin
    nlin = 1 + (NP - 1) / 3
    block(1:nlin) = line(n:n+nlin-1)
    call xyblock(block,nlin,x,y)
    do j = 1, NP
      E3(L,j) = x(j) * 1.e-6
      xs3(L,j) = y(j) * 1.e3
    enddo
    Nxs3(L)=NP
    if (NSt == 1 .and. L == Lbeg) then
      Liss(L) = min(LL,NSt)
    else
      Liss(L) = L
    endif
    n=n+nlin
!
! Cut off energies above EMAX
!
    do j = 1, NP
      if (E3(L,j) > EMAX) then
        Nxs3(L) = j - 1
        exit
      endif
    enddo
  enddo
  return
end subroutine readMF3
