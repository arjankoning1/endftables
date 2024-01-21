subroutine integralxs(MT)
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
  character*132      :: endffile
  character*6        :: finalnuclide
  character*3        :: massstring
  character(len=16)  :: reaction   ! reaction
  character(len=15)  :: col(5)    ! header
  character(len=15)  :: un(5)    ! header
  character(len=80)  :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer            :: MF
  integer            :: MT
  integer            :: L
  integer            :: LL
  integer            :: i 
  integer            :: N 
  integer            :: Z 
  integer            :: A 
  integer            :: Ncol
  integer            :: nen 
  integer            :: nen0
  real(sgl)          :: Ea1
  real(sgl)          :: Eb1
  real(sgl)          :: xsa
  real(sgl)          :: xsb
  real(sgl)          :: xsf
  real(sgl)          :: Efl
  real(sgl)          :: Eloc(0:numen)
  real(sgl)          :: xsloc(0:numen)
  real(sgl)          :: xsloclow(0:numen)
  real(sgl)          :: xslocupp(0:numen)
  real(sgl)          :: sacs(numspec)
  real(sgl)          :: sacslow(numspec)
  real(sgl)          :: sacsupp(numspec)
!
! *************** Exclude SACS for discrete levels **
!
  MF = 3
  if (MT >= 50 .and. MT <= 91) return
  if (MT >= 600 .and. MT <= 849) return
!
! *************** Calculate SACS by folding *********
!
! Interpolate cross section grid on the grid of the integral spectrum
!
  un = 'mb'
  col(1)='Spectrum'
  un(1)=''
  col(2)=''
  un(2)=''
  col(3)='xs'
  col(4)='xslow'
  col(5)='xsup'
  quantity='Spectrum averaged cross section (SACS)'
  do L = -1, Niso
    if (Nxs(L) == 0) cycle
    sacs = 0.
    sacslow = 0.
    sacsupp = 0.
    N = Nxs(L)
    do i = 1, Nspectrum
      Eloc(0) = 0.
      xsloc(0) = 0.
      xsloclow(0) = 0.
      xslocupp(0) = 0.
      do nen = 1, N
        Eloc(nen) = E(L,nen)
        xsloc(nen) = xs(L,nen)
        xsloclow(nen) = xslow(L,nen)
        xslocupp(nen) = xsupp(L,nen)
      enddo
      do nen = 1, Nenspectrum(i)
        Efl = Espectrum(i,nen)
        if (Efl >= Eloc(0) .and. Efl <= Eloc(N)) then
          call locate(Eloc, 0, N, Efl, nen0)
          Ea1 = Eloc(nen0)
          Eb1 = Eloc(nen0 + 1)
          xsa = xsloc(nen0)
          xsb = xsloc(nen0 + 1)
          call pol1(Ea1, Eb1, xsa, xsb, Efl, xsf)
          sacs(i) = sacs(i) + xsf * fspectrum(i,nen)
          if (covexist) then
            xsa = xsloclow(nen0)
            xsb = xsloclow(nen0 + 1)
            call pol1(Ea1, Eb1, xsa, xsb, Efl, xsf)
            sacslow(i) = sacslow(i) + xsf * fspectrum(i,nen)
            xsa = xslocupp(nen0)
            xsb = xslocupp(nen0 + 1)
            call pol1(Ea1, Eb1, xsa, xsb, Efl, xsf)
            sacsupp(i) = sacsupp(i) + xsf * fspectrum(i,nen)
          endif
        endif
      enddo
    enddo
!
! *** Write SACS
!
    if (L == -1) then
      endffile = trim(endfMT)//'-sacs.'//trim(lib)
      LL=L
    else
      LL=Liss(L)
      endffile = trim(endfMT)//isochar(LL)//'-sacs.'//trim(lib)
    endif
    Z = ZCN - Zix(MT)
    A = ACN - Zix(MT) - Nix(MT)
    massstring='   '
    write(massstring,'(i3)') A
    if (MT > 3 .and. Z > 0) then
      finalnuclide=trim(nuc(Z))//trim(adjustl(massstring))//isochar(L)
    else
      finalnuclide=''
    endif
    reaction=MTreac(MT,LL)
    topline=trim(targetnuclide)//trim(reaction)//trim(finalnuclide)//' '//trim(quantity)
    if (covexist) then
      Ncol=5
    else
      Ncol=3
    endif
    open (unit = 1, status = 'unknown', file = endffile)
    call write_header(topline,source,user,date,oformat)
    call write_endf(2,library,author,year)
    call write_target
    call write_reaction(reaction,0.D0,0.D0,MF,MT)
    if ( MT > 3 .and. Z > 0) call write_residual(Z,A,finalnuclide)
    call write_datablock(quantity,Ncol,Nspectrum,col,un)
    if (covexist) then
      do i = 1, Nspectrum
        write(1, '(a30, 3es15.6)') spectrum(i), sacs(i), sacslow(i), sacsupp(i)
      enddo
    else
      do i = 1, Nspectrum
        write(1, '(a30, es15.6)') spectrum(i), sacs(i)
      enddo
    endif
    close (1)
  enddo
end subroutine integralxs
! Copyright A.J. Koning 2021
