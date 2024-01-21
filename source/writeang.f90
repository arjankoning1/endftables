subroutine writeang(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write angular distributions
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
  character*7        :: Estring
  character*132      :: endffile
  character(len=16) :: reaction   ! reaction
  character(len=132) :: topline    ! topline
  character(len=13) :: Estr  
  character(len=15) :: col(2)     ! header
  character(len=15) :: un(2)     ! header
  character(len=80) :: quantity   ! quantity
  integer            :: MF
  integer            :: MT
  integer            :: nen
  integer            :: iang 
  integer            :: Ncol 
  real               :: EE
!
! *** Write angular distributions
!
  MF = 4
  Ncol=2
  col(1)='Angle'
  un(1)='deg'
  col(2)='xs'
  un(2)='mb/sr'
  do nen = 1, min(NE,numen6)
    Estring='000.000'
    EE=E4(nen)
    if (EE.ge.1.e-3.and.EE.lt.1.e3) then
      write(Estring(1:7),'(f7.3)') EE
      write(Estring(1:3),'(i3.3)') int(EE)
    else
      write(Estring(1:7),'(es7.1)') EE
    endif
    Estr=''
    write(Estr,'(es13.6)') EE
    endffile = trim(endfMT)//'-Eang'//Estring//'.'//trim(lib)
    open (unit = 1, status = 'unknown', file = endffile)
    quantity='angular distribution'
    reaction=MTreac(MT,-1)
    topline=trim(targetnuclide)//trim(reaction)//' '//trim(quantity)//' at '//Estr//' MeV'
    call write_header(topline,source,user,date,oformat)
    call write_endf(2,library,author,year)
    call write_target
    call write_reaction(reaction,0.D0,0.D0,MF,MT)
    call write_real(2,'E-incident [MeV]',EE)
    call write_datablock(quantity,Ncol,181,col,un)
    do iang=0,180
      write(1, '(2es15.6)') real(iang), angdis(nen,iang)
    enddo
    close (1)
  enddo
end subroutine writeang
