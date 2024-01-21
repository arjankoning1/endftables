subroutine readMF4(line,Nlines)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read MF4
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
  integer            :: n
  integer            :: LTT
  integer            :: LCT
  integer            :: NR
  integer            :: nlin
  integer            :: NRh
  integer            :: nen
  integer            :: L
  integer            :: NL
  integer            :: k
  real               :: EE
  real               :: x(3*Nlines)
  real               :: y(3*Nlines)
!
! Read MF4
!
  n=1
  read(line(n)(34:44), '(i11)') LTT
  if (LTT == 0) return
  if (LTT /= 2) then
    n=n+1
    read(line(n)(34:44), '(i11)') LCT
    n=n+1
    read(line(n)(45:55), '(i11)') NR 
    read(line(n)(56:66), '(i11)') NEl
    nlin = 1 + (NR - 1) / 3
    n=n+1+nlin
    do nen = 1, min(NEl,numen6)
      read(line(n)(12:22), '(e11.6)') EE
      E4l(nen) = EE * 1.e-6
      read(line(n)(45:55), '(i11)') NL
      nlin = 1 + (NL - 1) / 6
      block(1:nlin) = line(n+1:n+nlin)
      call yblock(block,nlin,y)
      cleg(nen,0)=1.
      do L = 1, NL
        cleg(nen,L) = y(L)
      enddo
      Nleg(nen)=NL
      n=n+1+nlin
    enddo
  endif
  if (LTT >= 2) then
    read(line(n)(45:55), '(i11)') NRh
    read(line(n)(55:66), '(i11)') NEh
    nlin = 1 + (NRh - 1) / 3
    n=n+1+nlin
    do nen = 1, NEh
      read(line(n)(12:22), '(e11.6)') EE
      E4h(nen) = EE * 1.e-6
      read(line(n)(45:55), '(i11)') NR
      read(line(n)(56:66), '(i11)') NAh(nen)
      nlin = 1 + (NR - 1) / 3
      n=n+1+nlin
      nlin = 1 + (NAh(nen) - 1) / 3
      block(1:nlin) = line(n:n+nlin-1)
      call xyblock(block,nlin,x,y)
      do k = 1, NAh(nen)
        mu4(nen,k) = x(k)
        ang4(nen,k) = y(k)
      enddo
      n=n+nlin
    enddo
  endif
  return
end subroutine readMF4
