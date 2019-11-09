module AgSysExcepUtils
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Utils and params needed by AgSys
  !
  ! !USES:
  use shr_log_mod           , only : errMsg => shr_log_errMsg
  use abortutils            , only : endrun
  use clm_varctl            , only : iulog 
  !
  implicit none
  private

  ! !PUBLIC DATA:
  public :: errMsg
  public :: endrun
  public :: iulog
end module AgSysExcepUtils
