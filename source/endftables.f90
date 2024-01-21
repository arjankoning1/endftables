program endftables
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Translate ENDF-6 file into x-y tables
!
! Revision    Date      Author           Description
! ====================================================
!    1     2023-12-29   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
!   |-------------------------------------------------------|
!   |                 Arjan Koning                          |
!   |                                                       |
!   | Email: A.Koning@@iaea.org                             |
!   |-------------------------------------------------------|
!
! MIT License
!
! Copyright (c) 2023 Arjan Koning
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
  use endftables_mod
!
! *** Declaration of local data
!
  implicit none
  integer            :: MT                                   !
  integer            :: MF                                   !
  integer            :: NMTlines                             !
!
! Input options
!
  flagangle=.true.
  call readinput
  call input
  call checkkeyword
!
! Reading and initialization
!
  call reactioncodes
  call readendf
  call initial
!
! Main info from MF1-MT451
!
  MF = 1
  MT = 451
  if (Rmfmtexist(MF,MT)) then
    NMTlines=sectionend(MF,MT)-sectionbeg(MF,MT)+1
    MTline(1:NMTlines)=endfline(sectionbeg(MF,MT):sectionend(MF,MT))
    call readMF1(MTline,NMTlines,MT)
    call MTinitial(MT)
    call writeMT(MTline,NMTlines,MF,MT)
  endif
!
! Read and process MF2 and MF32
!
  do MF = 2, 32, 30
    MT = 151
    if (Rmfmtexist(MF,MT)) then
      NMTlines=sectionend(MF,MT)-sectionbeg(MF,MT)+1
      MTline(1:NMTlines)=endfline(sectionbeg(MF,MT):sectionend(MF,MT))
      call MTinitial(MT)
      call writeMT(MTline,NMTlines,MF,MT)
    endif
  enddo
!
! Read and process MT5
!
  MT = 5
  call MTinitial(MT)
  MF = 3
  if (Rmfmtexist(MF,MT)) then
    NMTlines=sectionend(MF,MT)-sectionbeg(MF,MT)+1
    MTline(1:NMTlines)=endfline(sectionbeg(MF,MT):sectionend(MF,MT))
    call readMF3(MTline,NMTlines,MF,MT)
    call processMF3(MT)
    call writexs(MT)
  endif
  MF = 6
  if (Rmfmtexist(MF,MT)) then
    NMTlines=sectionend(MF,MT)-sectionbeg(MF,MT)+1
    MTline(1:NMTlines)=endfline(sectionbeg(MF,MT):sectionend(MF,MT))
    call readMF6(MTline,NMTlines,MT)
    call processMF6(MT)
  endif
  if (proj == 'n') call integral
!
! Read other MF/MT combinations
!
  do MT=1,nummt
    if (.not. mtexist(MT)) cycle
    if (MT == 451 .or. MT == 5) cycle
    call MTinitial(MT)
    do MF=1,nummf
      if (.not. Rmfmtexist(MF,MT)) cycle
      NMTlines=sectionend(MF,MT)-sectionbeg(MF,MT)+1
      MTline(1:NMTlines)=endfline(sectionbeg(MF,MT):sectionend(MF,MT))
      if (MF == 1) call readMF1(MTline,NMTlines,MT)
      if (MF == 3 .or. MF == 9 .or. MF == 10) call readMF3(MTline,NMTlines,MF,MT)
      if (flagangle .and. MF == 4) call readMF4(MTline,NMTlines)
      if (MF == 6) call readMF6(MTline,NMTlines,MT)
      if (MF == 33 .or. MF == 40) call readMF33(MTline,NMTlines,MF)
    enddo
!
! Process MF/MT combinations
!
    if (mfmtexist(9,MT)) call processMF9
    do MF=1,nummf
      if (.not. mfmtexist(MF,MT)) cycle
      if (flagangle .and. MF == 4) call processMF4
      if (MF == 6) call processMF6(MT)
    enddo
    if (mfmtexist(3,MT) .or. mfmtexist(10,MT)) call processMF3(MT)
    if (mfmtexist(33,MT) .or. mfmtexist(40,MT)) call processMF33
!
! Output data
!
    do MF=1,nummf
      if (.not. mfmtexist(MF,MT)) cycle
      if (MF == 1) call writenu(MT)
      if (MF == 3 .or. MF == 9 .or. MF == 10) then
        call writexs(MT)
        if (Nspectrum > 0) call integralxs(MT)
      endif
      if (flagangle .and. MF == 4) call writeang(MT)
    enddo
  enddo
  call residual
  call production
end
