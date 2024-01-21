subroutine writexs(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write cross sections
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
  logical            :: flagfission
  character*3        :: massstring
  character*6        :: finalnuclide
  character*132      :: endffile
  character(len=16)  :: reaction   ! reaction
  character(len=15)  :: col(4)    ! header
  character(len=15)  :: un(4)    ! header
  character(len=80)  :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: MF
  integer            :: MT
  integer            :: L
  integer            :: Z
  integer            :: A
  integer            :: LL
  integer            :: k 
  integer            :: Ncol
!
! *** Write cross sections
!
  un = 'mb'
  col(1)='E'
  un(1)='MeV'
  col(2)='xs'
  col(3)='xslow'
  col(4)='xsup'
  quantity='cross section'
  flagfission = (MT == 18 .or. MT == 19 .or. MT == 20 .or. MT == 21 .or. MT == 38)
  do L = -1, Niso
    if (Nxs(L) == 0) cycle
    if (L == -1) then
      endffile = trim(endfMT)//'.'//trim(lib)
      LL=L
      MF=3
    else
      LL=Liss(L)
      endffile = trim(endfMT)//isochar(LL)//'.'//trim(lib)
      MF=10
    endif
    Z = ZCN - Zix(MT)
    A = ACN - Zix(MT) - Nix(MT)
    massstring='   '
    write(massstring,'(i3)') A
    if (Z > 0 .and. MT > 3 .and. .not.flagfission) then
      finalnuclide=trim(nuc(Z))//trim(adjustl(massstring))//isochar(L)
    else
      finalnuclide=''
    endif
    reaction=MTreac(MT,LL)
    topline=trim(targetnuclide)//trim(reaction)//trim(finalnuclide)//' '//trim(quantity)
    open (unit = 1, status = 'unknown', file = endffile)
    call write_header(topline,source,user,date,oformat)
    call write_endf(2,library,author,year)
    call write_target
    call write_reaction(reaction,dble(QI(L)),dble(E(L,1)),MF,MT)
    if (Z > 0 .and. MT > 3 .and. .not.flagfission) then
      call write_residual(Z,A,finalnuclide)
      if (L /= -1) call write_level(2,L,-1,0.,-1.,0,0.)
    endif
    if (covexist) then
      Ncol=4
    else
      Ncol=2
    endif
    call write_datablock(quantity,Ncol,Nxs(L),col,un)
    if (covexist) then
      do k = 1, Nxs(L)
        write(1, '(4es15.6)') E(L,k), xs(L,k), xslow(L,k), xsupp(L,k)
      enddo
    else
      do k = 1, Nxs(L)
        write(1, '(2es15.6)') E(L,k), xs(L,k)
      enddo
    endif
    close (1)
  enddo
end subroutine writexs
