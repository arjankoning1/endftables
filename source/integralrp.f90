subroutine integralrp(k)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Integral cross sections per residual product
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
  character*14       :: sacsstring
  character*132      :: endffile
  character*6        :: finalnuclide
  character*3        :: massstring
  character(len=16)  :: reaction   ! reaction
  character(len=15)  :: col(3)    ! header
  character(len=15)  :: un(3)    ! header
  character(len=80)  :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: k 
  integer            :: MF 
  integer            :: MT 
  integer            :: Z
  integer            :: A
  integer            :: i
  integer            :: iso
  integer            :: Ncol
  integer            :: nen0
  integer            :: nen
  real(sgl)          :: Ea1
  real(sgl)          :: Eb1
  real(sgl)          :: xsa
  real(sgl)          :: xsb
  real(sgl)          :: xsf
  real(sgl)          :: Efl
  real(sgl)          :: sacs(numspec)
!
! *************** Calculate SACS by folding *********
!
! Interpolate cross section grid on the grid of the integral spectrum
!
  MF = 6
  sacs = 0.
  do i = 1, Nspectrum
    do nen = 1, Nenspectrum(i)
      Efl = Espectrum(i,nen)
      if (Efl >= Eres(0) .and. Efl <= Eres(Nresi)) then
        call locate(Eres, 0, Nresi, Efl, nen0)
        Ea1 = Eres(nen0)
        Eb1 = Eres(nen0 + 1)
        xsa = xsres(nen0)
        xsb = xsres(nen0 + 1)
        call pol1(Ea1, Eb1, xsa, xsb, Efl, xsf)
        sacs(i) = sacs(i) + xsf * fspectrum(i,nen)
      endif
    enddo
  enddo
!
! *** Write SACS
!
  un = ''
  col(1)='Spectrum'
  col(2)=''
  col(3)='xs'
  un(3)='mb'
  Ncol=3
  quantity='Spectrum averaged cross section (SACS)'
  MT = 5
  Z = Zres(k)
  A = Ares(k)
  if (Z <= 2 .and. A <= 4 ) return
  sacsstring='rp000000 '
  write(sacsstring(3:5),'(i3.3)') Z
  write(sacsstring(6:8),'(i3.3)') A
  if (Nisorp(Z,A) == 0) then
    iso = -1
  else
    iso = LIP(MT,k)
  endif
  sacsstring(9:9)=isochar(iso)
  sacsstring=trim(sacsstring)//'-sacs'
  endffile = trim(endfhead)//'-'//trim(sacsstring)//'.'//trim(lib)
  massstring='   '
  write(massstring,'(i3)') A
  finalnuclide=trim(nuc(Z))//trim(adjustl(massstring))//isochar(iso)
  reaction='('//proj//',x)'
  topline=trim(targetnuclide)//trim(reaction)//trim(finalnuclide)//' '//trim(quantity)
  open (unit = 1, status = 'unknown', file = endffile)
  call write_header(topline,source,user,date,oformat)
  call write_endf(2,library,author,year)
  call write_target
  call write_reaction(reaction,0.D0,0.D0,MF,MT)
  call write_residual(Z,A,finalnuclide)
  call write_datablock(quantity,Ncol,Nspectrum,col,un)
  do i = 1, Nspectrum
    write(1, '(a30, es15.6)') spectrum(i), sacs(i)
  enddo
  close (1)
end subroutine integralrp
! Copyright A.J. Koning 2021
