subroutine writenu(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write nubar
!
! Revision    Date      Author           Description
! ====================================================
!    1     2023-08-08   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use endftables_mod
!
! *** Declaration of local data
!
  implicit none
  character*8        :: nustring
  character*132      :: endffile
  character(len=16)  :: reaction   ! reaction
  character(len=15)  :: col(2)    ! header
  character(len=15)  :: un(2)    ! header
  character(len=80)  :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: MF
  integer            :: MT
  integer            :: nutype 
  integer            :: k 
  integer            :: Ncol
!
! *** Write nubar
!
  MF = 1
  col(1)='E'
  un(1)='MeV'
  col(2)='nubar'
  un(2)=''
  Ncol=2
  endffile = trim(endfMT)//'.'//trim(lib)
  if (MT == 452) then
    nutype=1
    nustring='total   '
  endif
  if (MT == 455) then
    nutype=2
    nustring='delayed '
  endif
  if (MT == 456) then
    nutype=3
    nustring='prompt  '
  endif
  quantity=trim(nustring)//' nubar'
  reaction='(n,f)'
  topline=trim(targetnuclide)//trim(reaction)//' '//trim(quantity)
  open (unit = 1, status = 'unknown', file = endffile)
  if (MT <= 456) then
    call write_header(topline,source,user,date,oformat)
    call write_endf(2,library,author,year)
    call write_target
    call write_reaction(reaction,0.D0,0.D0,MF,MT)
    call write_datablock(quantity,Ncol,Nnubar(nutype),col,un)
    do k = 1, Nnubar(nutype)
      write(1, '(2es15.6)') Enubar(nutype,k), nubar(nutype,k)
    enddo
  endif
  if (MT == 458) then
    quantity='Fission energy release'
    topline=trim(targetnuclide)//trim(reaction)//' '//trim(quantity)
    call write_header(topline,source,user,date,oformat)
    call write_endf(2,library,author,year)
    call write_target
    call write_reaction(reaction,0.D0,0.D0,MF,MT)
    write(1,'("# observables:")')
    call write_real(2,'EFR - kinetic energy of fragments [eV]',EFR)
    call write_real(2,'DEFR - kinetic energy of fragments uncertainty [eV]',DEFR)
    call write_real(2,'ENP - kinetic energy of prompt neutrons [eV]',ENP)
    call write_real(2,'DENP - kinetic energy of prompt neutons uncertainty [eV]',DENP)
    call write_real(2,'END - kinetic energy of delayed neutrons [eV]',END1)
    call write_real(2,'DEND - kinetic energy of delayed neutrons uncertainty [eV]',DEND)
    call write_real(2,'EGP - total energy of prompt gamma rays [eV]',EGP)
    call write_real(2,'DEGP - total energy of prompt gamma rays uncertainty [eV]',DEGP)
    call write_real(2,'EGD - total energy of delayed gamma rays [eV]',EGD)
    call write_real(2,'DEGD - total energy of delayed gamma rays uncertainty [eV]',DEGD)
    call write_real(2,'EBDEL - total energy of delayed betas [eV]',EBDEL)
    call write_real(2,'DEBDEL - total energy of delayed betas uncertainty [eV]',DEBDEL)
    call write_real(2,'ENU - total energy of neutrinos [eV]',ENU)
    call write_real(2,'DENU - total energy of neutrinos uncertainty [eV]',DENU)
    call write_real(2,'ERN - total energy - energy of neutrinos [eV]',ERN)
    call write_real(2,'DERN - total energy - energy of neutrinos uncertainty [eV]',DERN)
  endif
  close (1)
end subroutine writenu
