subroutine integral
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read integral spectrum
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     29-06-2021   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use endftables_mod
!
! *** Declaration of local data
!
  implicit none
  character(len=132) :: home                 ! home directory
  character(len=132) :: spectrumdir          ! spectra directory
  character(len=132) :: spectrumlist         ! list with spectrum files
  character(len=132) :: specfile             ! spectrum file
  integer            :: i                    ! counter
  integer            :: igr                  ! 
  integer            :: istat                ! logical for file access
  integer            :: nen                  ! energy counter
  real               :: Eup                  ! upper energy of bin for spectrum
  real               :: Elow                 ! lower energy of bin for spectrum
  real               :: specsum              ! check for conservation of flux
  real               :: fspec(numenspec)     ! spectrum values
  real               :: dum                  ! 
  real               :: sp                   ! 
!
! ***************************** Set directories ************************
!
  home='/Users/koning/'
  spectrumdir=trim(home)//'talys/structure/integral/spectra/'
  spectrumlist=trim(spectrumdir)//'spectrum.list'
!
! **************************** Read list of spectra ********************
!
  Nspectrum = 0
  open (unit=1, file=spectrumlist, status='old', iostat = istat)
  if (istat /= 0) return
  i = 1
  do
    read(1, '(a)', iostat = istat) spectrum(i)
    if (istat /= 0) exit
    i = i + 1
  enddo
  close (unit = 1)
  Nspectrum = i - 1
!
! ********* Read integral spectrum from database **********
!
  Nenspectrum = 0
  Espectrum = 0
  fspectrum = 0
  do i = 1, Nspectrum
    specfile = trim(spectrumdir)//trim(spectrum(i))//'.txt'
    open (unit = 2, file = specfile, status = 'old', iostat = istat)
    if (istat == 0) then
      read(2, '()') 
      read(2, '()') 
      fspec = 0.
      specsum = 0.
      nen=0
      do 
        read(2, *, iostat=istat ) igr, Eup, Elow, dum, sp
        if (istat /= 0) exit
        nen = nen + 1
        Espectrum(i,nen) = 0.5 * (Eup + Elow) * 1.e-6
        fspec(nen) = sp
        specsum = specsum + fspec(nen)
      enddo
      close (unit = 2)
      Nenspectrum(i) = nen
      do nen = 1, Nenspectrum(i)
        fspectrum(i,nen) = fspec(nen) / specsum
      enddo
    endif
  enddo
  return
end subroutine integral
! Copyright A.J. Koning 2021
