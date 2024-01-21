subroutine readMF6(line,Nlines,MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read MF6
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
  integer            :: MT
  integer            :: LCT
  integer            :: n
  integer            :: k
  integer            :: LAW
  integer            :: NR
  integer            :: NP
  integer            :: nlin
  integer            :: nen
  integer            :: nen2
  integer            :: LANG
  integer            :: LEP
  integer            :: LIDP
  integer            :: NA
  integer            :: ND
  integer            :: NW
  integer            :: L
  integer            :: NL
  integer            :: NRP0
  integer            :: NEP
  integer            :: LTP
  integer            :: NRM
  integer            :: NMU
  real               :: x(6*Nlines)
  real               :: y(6*Nlines)
  real               :: EE
  real               :: SPIPAR
!
! Read yields
!
  n=1
  read(line(n)(34:44), '(i11)') LCT
  read(line(n)(45:55), '(i11)') NK
  NK = min (NK, numsec)
  if (MT == 2) then
    allocate(b6(NK,1,12*numang))
  else
    allocate(b6(NK,1,numen6))
  endif
  b6=0.
  n=n+1
  do k = 1, NK
    read(line(n)(1:11), '(es11.6)') ZAP(k)
    read(line(n)(12:22), '(es11.6)') AWP(k)
    read(line(n)(23:33), '(i11)') LIP(MT,k)
    read(line(n)(34:44), '(i11)') LAW
    read(line(n)(45:55), '(i11)') NR
    read(line(n)(56:66), '(i11)') NP
    nlin = 1 + (NR - 1) / 3
    n=n+1+nlin
    nlin = 1 + (NP - 1) / 3
    block(1:nlin) = line(n:n+nlin-1)
    call xyblock(block,nlin,x,y)
    if (MT == 5) then
      do nen = 1, NP
        EY6(k,nen) = x(nen) * 1.e-6
        Y6(k,nen) = y(nen)
      enddo
      NY6(k)=NP
    endif
    n=n+nlin
!
! Read distributions
!
    NR = 1
    if (LAW == 1 .or. LAW == 2 .or. LAW == 4 .or. LAW == 5 .or. LAW == 7) then
      if (LAW == 1) then
        read(line(n)(23:33), '(i11)') LANG
        read(line(n)(34:44), '(i11)') LEP
        read(line(n)(45:55), '(i11)') NR
        read(line(n)(56:66), '(i11)') NP
      endif
      if (LAW == 2 .or. LAW == 4) then
        read(line(n)(45:55), '(i11)') NR
        read(line(n)(56:66), '(i11)') NP
      endif
      if (LAW == 5) then
        read(line(n)(1:11), '(e11.6)') SPIPAR
        read(line(n)(34:44), '(i11)') LIDP
        read(line(n)(45:55), '(i11)') NR
        read(line(n)(56:66), '(i11)') NP
      endif
      if (LAW == 7) then
        read(line(n)(45:55), '(i11)') NR
        read(line(n)(56:66), '(i11)') NP
      endif
      nlin = 1 + (NR - 1) / 3
      n=n+1+nlin
      do nen = 1, NP
        EE = 0.
        NW = 0
        if (LAW == 1) then
          read(line(n)(12:22), '(e11.6)') EE
          read(line(n)(23:33), '(i11)') ND
          read(line(n)(34:44), '(i11)') NA
          read(line(n)(45:55), '(i11)') NW
          read(line(n)(56:66), '(i11)') NEP
        endif
        if (LAW == 2) then
          read(line(n)(12:22), '(e11.6)') EE
          read(line(n)(23:33), '(i11)') LANG
          read(line(n)(45:55), '(i11)') NW
          read(line(n)(56:66), '(i11)') NL
        endif
        if (LAW == 4) then
          n = n + 1
          cycle
        endif
        if (LAW == 5) then
          read(line(n)(12:22), '(e11.6)') EE
          read(line(n)(23:33), '(i11)') LTP
          read(line(n)(45:55), '(i11)') NW
          read(line(n)(56:66), '(i11)') NL
        endif
        if (LAW == 7) then
          read(line(n)(12:22), '(e11.6)') EE
          read(line(n)(45:55), '(i11)') NRM
          read(line(n)(56:66), '(i11)') NMU
          nlin = 1 + (NRM - 1) / 3
          n=n+1+nlin
          do nen2 = 1, NMU
            read(line(n)(45:55), '(i11)') NRP0
            read(line(n)(56:66), '(i11)') NEP
            nlin = 1 + (NRP0 - 1) / 3
            n=n+1+nlin
            nlin = 1 + (NEP - 1) / 3
            n=n+nlin
          enddo
        else
          E6(k,nen) = EE * 1.e-6
          nlin = 1 + (NW - 1) / 6
          block(1:nlin) = line(n+1:n+nlin)
          call yblock(block,nlin,y)
          if (LAW == 2) then
            cleg(nen,0)=1.
            do L = 1, NW
              cleg(nen,L) = y(L)
            enddo
            Nleg(nen)=NL
          else
            do L = 1, NW
              b6(k,1,L) = y(L)
            enddo
            Nb6(k,nen)=NW
          endif
          n=n+1+nlin
        endif
      enddo
    endif
  enddo
  return
end subroutine readMF6
