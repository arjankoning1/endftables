subroutine writerp(k)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write residual production cross sections
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
  character*9        :: rpstring
  character*132      :: endffile
  character*6    :: finalnuclide
  character*3        :: massstring
  character(len=16) :: reaction   ! reaction
  character(len=15) :: col(2)    ! header
  character(len=15) :: un(2)    ! header
  character(len=80) :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: k 
  integer            :: MF 
  integer            :: MT 
  integer            :: Z
  integer            :: A
  integer            :: Ncol
  integer            :: iso
  integer            :: nen
!
! *** Write cross sections
!
  MF = 6
  MT = 5
  Z = Zres(k)
  A = Ares(k)
  if (Z <= 2 .and. A <= 4 ) return
  rpstring='rp000000 '
  write(rpstring(3:5),'(i3.3)') Z
  write(rpstring(6:8),'(i3.3)') A
  if (Nisorp(Z,A) == 0) then
    iso = -1
  else
    iso = LIP(MT,k)
  endif
  rpstring(9:9)=isochar(iso)
  endffile = trim(endfhead)//'-'//trim(rpstring)//'.'//trim(lib)
  massstring='   '
  write(massstring,'(i3)') A
  finalnuclide=trim(nuc(Z))//trim(adjustl(massstring))//isochar(iso)
  quantity='cross section'
  reaction='('//proj//',x)'
  topline=trim(targetnuclide)//trim(reaction)//trim(finalnuclide)//' '//trim(quantity)
  col(1)='E'
  un(1)='MeV'
  col(2)='xs'
  un(2)='mb'
  Ncol=2
  open (unit = 1, status = 'unknown', file = endffile)
  call write_header(topline,source,user,date,oformat)
  call write_endf(2,library,author,year)
  call write_target
  call write_reaction(reaction,0.D0,0.D0,MF,MT)
  call write_residual(Z,A,finalnuclide)
  if (iso /= -1) call write_level(2,iso,-1,0.,-1.,0,0.)
  call write_datablock(quantity,Ncol,Nresi,col,un)
  do nen = 1, Nresi
    write(1, '(2es15.6)') Eres(nen), xsres(nen)
  enddo
  close (1)
end subroutine writerp
