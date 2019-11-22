module AgSysPhysicalConsts 
  use AgSysKinds,    only : r8
  use shr_const_mod, only : SHR_CONST_TKFRZ, SHR_CONST_CDAY
  implicit none

  private

  public :: SHR_CONST_TKFRZ
  real(r8), parameter, public :: SHR_CONST_SECONDS_PER_HOUR = SHR_CONST_CDAY/24
end module AgSysPhysicalConsts 
