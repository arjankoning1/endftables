subroutine readMF33(line,Nlines,MF)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read MF33 and MF40
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
  integer            :: n
  integer            :: Nis
  integer            :: LB
  integer            :: NT
  integer            :: nlin
  integer            :: istat
  integer            :: k
  integer            :: L
  integer            :: Lbeg
  integer            :: Lend
  integer            :: kcov
  real               :: y(6*Nlines)
!
! Read MF33
!
  if (Nlines <= 2) return
  if (MF == 33) then
   Lbeg = -1
   Lend = -1
   n=3
  else
   read(line(1)(45:55), '(i11)') Nis
   Lbeg = 0
   Lend = Nis - 1
   n=4
  endif
  do L = Lbeg, Lend
    read(line(n)(34:66), '(3i11)', iostat=istat) LB,NT,NEcov(L)
    if (istat /= 0) exit
    if (LB /= 5) cycle
    n = n + 1
    nlin = 1 + (NT - 1) / 6
    block(1:nlin) = line(n:n+nlin-1)
    call yblock(block,nlin,y)
    NEcov(L) = min(NEcov(L), numcov)
    do k = 1, NEcov(L)
      Ecov(L, k) = y(k) * 1.e-6
    enddo
    do k = NEcov(L) + 1, NT
      kcov = k - NEcov(L)
      ycov(L, kcov) = y(k)
    enddo
    n = n + nlin + 2
  enddo
  return
end subroutine readMF33
