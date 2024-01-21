subroutine writeprod(type)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write particle production cross sections
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
  character*132      :: endffile
  character(len=16) :: reaction   ! reaction
  character(len=15) :: col(2)    ! header
  character(len=15) :: un(2)    ! header
  character(len=80) :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: MF
  integer            :: MT
  integer            :: type 
  integer            :: L 
  integer            :: Ncol
  integer            :: nen
!
! *** Write cross sections
!
  MF = 3
  col(1)='E'
  un(1)='MeV'
  col(2)='xs'
  un(2)='mb'
  Ncol=2
  quantity='cross section'
  if (type == 1) MT=201
  if (type == 0) MT=202
  if (type == 2) MT=203
  if (type == 3) MT=204
  if (type == 4) MT=205
  if (type == 5) MT=206
  if (type == 6) MT=207
  MTreac(MT,-1)(2:2) = proj
  L = len_trim(endfMT)
  write(endfMT(L-2:L),'(i3)') MT
  endffile = trim(endfMT)//'.'//trim(lib)
  reaction=MTreac(MT,-1)
  topline=trim(targetnuclide)//trim(reaction)//' '//trim(quantity)
  open (unit = 1, status = 'unknown', file = endffile)
  call write_header(topline,source,user,date,oformat)
  call write_endf(2,library,author,year)
  call write_target
  call write_reaction(reaction,0.D0,0.D0,MF,MT)
  call write_datablock(quantity,Ncol,Nprod(type),col,un)
  do nen = 1, Nprod(type)
    write(1, '(2es15.6)') Eprod(type,nen), xsprod(type,nen)
  enddo
  close (1)
end subroutine writeprod
