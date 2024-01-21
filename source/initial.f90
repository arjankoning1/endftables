subroutine initial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialize data
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
  integer                :: iang
  integer                :: L
  integer                :: i
  integer                :: MT
  integer                :: type
  integer                :: Npar(6)
  integer                :: is
  integer                :: yy
  integer                :: month
  integer                :: day
  integer                :: values(8) ! date and time values
  real                   :: angle
  real                   :: mu
  real                   :: plegendre
!
! ************************ Nuclear symbols *****************************
!
  nuc =                              (/ 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
    'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
    'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
    'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
    'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
    'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds', &
    'Rg', 'Cn', 'Nh', 'Fl', 'Mc', 'Lv', 'Ts', 'Og', 'B9', 'C0', 'C1', 'C2', 'C3', 'C4'/)
   parsym = (/ 'g', 'n', 'p', 'd', 't', 'h', 'a'/)
   parZ = (/ 0, 0, 1, 1, 1, 2, 2/)
   parN = (/ 0, 1, 0, 1, 2, 1, 2/)
   parA = (/ 0, 1, 1, 2, 3, 3, 4/)
   isochar = (/ ' ', 'g', 'm', 'n', 'o', 'p' /)
!  isochar = (/ ' ', 'g', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v' /)
!
! *** Initialization
!
!
! Set date
!
  call date_and_time(VALUES=values)
  yy=values(1)
  month=values(2)
  day=values(3)
  date='xxxx-xx-xx'
  write(date(1:4),'(i4.4)') yy
  write(date(6:7),'(i2.2)') month
  write(date(9:10),'(i2.2)') day
  proj = 'n'
!
! Legendre coefficients
!
  pi=3.14159265358979323
  deg2rad=pi/180.
  leg=0.
  if (flagangle) then
    do iang=0,180
      angle=real(iang)*deg2rad
      mu=cos(angle)
      do L=0,numleg
        leg(L,iang)=plegendre(L,mu)
      enddo
    enddo
  endif
!
! MT numbers
!
  MTcode=-1
  MTcode(4)=100000
  MTcode(11)=201000
  MTcode(16)=200000
  MTcode(17)=300000
  MTcode(22)=100001
  MTcode(23)=100003
  MTcode(24)=200001
  MTcode(25)=300001
  MTcode(28)=110000
  MTcode(29)=100002
  MTcode(30)=200002
  MTcode(32)=101000
  MTcode(33)=100100
  MTcode(34)=100010
  MTcode(35)=101002
  MTcode(36)=100102
  MTcode(37)=400000
  MTcode(41)=210000
  MTcode(42)=310000
  MTcode(44)=120000
  MTcode(45)=110001
! MTcode(101)=000000
  MTcode(102)=000000
  MTcode(103)=010000
  MTcode(104)=001000
  MTcode(105)=000100
  MTcode(106)=000010
  MTcode(107)=000001
  MTcode(108)=000002
  MTcode(109)=000003
  MTcode(111)=020000
  MTcode(112)=010001
  MTcode(113)=000102
  MTcode(114)=001002
  MTcode(115)=011000
  MTcode(116)=010100
  MTcode(117)=001001
  Atarget = 0
  LISO = 0
  Liss = 0
  El = '  '
  Zix = 0
  Nix = 0
  Zres = 0
  Ares = 0
  Nres = 0
  pprod = 0
  do MT = 1, 200
    if (MTcode(MT) == -1) cycle
    Npar(1)=MTcode(MT)/100000
    Npar(2)=(MTcode(MT)-Npar(1)*100000)/10000
    Npar(3)=(MTcode(MT)-Npar(1)*100000-Npar(2)*10000)/1000
    Npar(4)=(MTcode(MT)-Npar(1)*100000-Npar(2)*10000-Npar(3)*1000)/100
    Npar(5)=(MTcode(MT)-Npar(1)*100000-Npar(2)*10000-Npar(3)*1000-Npar(4)*100)/10
    Npar(6)=MTcode(MT)-Npar(1)*100000-Npar(2)*10000-Npar(3)*1000-Npar(4)*100-Npar(5)*10
    Zix(MT) = 0
    Nix(MT) = 0
    do type = 1, 6
      Zix(MT) = Zix(MT) + parZ(type)*Npar(type)
      Nix(MT) = Nix(MT) + parN(type)*Npar(type)
      pprod(MT, type) = Npar(type)
    enddo
  enddo
  do MT = 50, 91
    Zix(MT) = 0
    Nix(MT) = 1
  enddo
  do MT = 600, 649
    Zix(MT) = 1
    Nix(MT) = 0
  enddo
  do MT = 650, 699
    Zix(MT) = 1
    Nix(MT) = 1
  enddo
  do MT = 700, 749
    Zix(MT) = 1
    Nix(MT) = 2
  enddo
  do MT = 750, 799
    Zix(MT) = 2
    Nix(MT) = 1
  enddo
  do MT = 800, 849
    Zix(MT) = 2
    Nix(MT) = 2
  enddo
  do MT = 600, 649
    Zix(MT) = 1
    Nix(MT) = 0
  enddo
  Zix(201) = 0
  Nix(201) = 1
  Zix(202) = 0
  Nix(202) = 0
  Zix(203) = 1
  Nix(203) = 0
  Zix(204) = 1
  Nix(204) = 1
  Zix(205) = 1
  Nix(205) = 2
  Zix(206) = 2
  Nix(206) = 1
  Zix(207) = 2
  Nix(207) = 2
!
! MTreac: reaction string
!
  MTreac='          '
  MTreac(1,-1)='(n,tot)   '
  MTreac(2,-1)='(n,el)    '
  MTreac(3,-1)='(n,non)   '
  MTreac(4,-1)="(n,n')    "
  MTreac(5,-1)="(n,any)   "
  MTreac(11,-1)='(n,2nd)   '
  MTreac(16,-1)='(n,2n)    '
  MTreac(17,-1)='(n,3n)    '
  MTreac(18,-1)='(n,f)     '
  MTreac(22,-1)='(n,na)    '
  MTreac(23,-1)='(n,n3a)   '
  MTreac(24,-1)='(n,2na)   '
  MTreac(25,-1)='(n,3na)   '
  MTreac(28,-1)='(n,np)    '
  MTreac(29,-1)='(n,n2a)   '
  MTreac(30,-1)='(n,2n2a)  '
  MTreac(32,-1)='(n,nd)    '
  MTreac(33,-1)='(n,nt)    '
  MTreac(34,-1)='(n,nh)    '
  MTreac(35,-1)='(n,nd2a)  '
  MTreac(36,-1)='(n,nt2a)  '
  MTreac(37,-1)='(n,4n)    '
  MTreac(41,-1)='(n,2np)   '
  MTreac(42,-1)='(n,3np)   '
  MTreac(44,-1)='(n,n2p)   '
  MTreac(45,-1)='(n,npa)   '
  MTreac(50,-1)="(n,n'_00)"
  do i=1,40
    MTreac(50+i,-1)="(n,n'_01) "
    write(MTreac(50+i,-1)(7:8),'(i2.2)') i
  enddo
  MTreac(91,-1)="(n,n'_con)"
! MTreac(101,-1)='(n,abs)   '
  MTreac(102,-1)='(n,g)     '
  MTreac(103,-1)='(n,p)     '
  MTreac(104,-1)='(n,d)     '
  MTreac(105,-1)='(n,t)     '
  MTreac(106,-1)='(n,h)     '
  MTreac(107,-1)='(n,a)     '
  MTreac(108,-1)='(n,2a)    '
  MTreac(109,-1)='(n,3a)    '
  MTreac(111,-1)='(n,2p)    '
  MTreac(112,-1)='(n,pa)    '
  MTreac(113,-1)='(n,t2a)   '
  MTreac(114,-1)='(n,d2a)   '
  MTreac(115,-1)='(n,pd)    '
  MTreac(116,-1)='(n,pt)    '
  MTreac(117,-1)='(n,da)    '
  MTreac(201,-1)='(n,xn)    '
  MTreac(202,-1)='(n,xg)    '
  MTreac(203,-1)='(n,xp)    '
  MTreac(204,-1)='(n,xd)    '
  MTreac(205,-1)='(n,xt)    '
  MTreac(206,-1)='(n,xh)    '
  MTreac(207,-1)='(n,xa)    '
  do i=0,40
    MTreac(600+i,-1)="(n,p_00)  "
    write(MTreac(600+i,-1)(6:7),'(i2.2)') i
    MTreac(650+i,-1)="(n,d_00)  "
    write(MTreac(650+i,-1)(6:7),'(i2.2)') i
    MTreac(700+i,-1)="(n,t_00)  "
    write(MTreac(700+i,-1)(6:7),'(i2.2)') i
    MTreac(750+i,-1)="(n,h_00)  "
    write(MTreac(750+i,-1)(6:7),'(i2.2)') i
    MTreac(800+i,-1)="(n,a_00)  "
    write(MTreac(800+i,-1)(6:7),'(i2.2)') i
  enddo
  MTreac(649,-1)='(n,p_con) '
  MTreac(699,-1)='(n,d_con) '
  MTreac(749,-1)='(n,t_con) '
  MTreac(799,-1)='(n,h_con) '
  MTreac(849,-1)='(n,a_con) '
  MTreac(998,-1)='(n,x)     '
  do MT=1,nummt
    do is=0,numiso
      MTreac(MT,is)=MTreac(MT,-1)
    enddo
  enddo
  NKres=0
  EY6=0
  NY6=0
  rpix=0
  MTix=0
  Nrpix=0
  Erp=0.
  xsrp=0.
  LIP=-1
  Nisom=-1
  Nprod=0
  Eprod=0.
  xsprod=0.
  return
end subroutine initial
