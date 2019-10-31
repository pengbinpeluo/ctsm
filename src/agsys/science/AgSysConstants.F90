module AgSysConstants

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Constants needed throughout AgSys
  !
  ! !USES:

  !
  implicit none
  private

  ! !PUBLIC DATA:

  ! Crop types
  integer, parameter, public :: crop_type_maize = 1
  integer, parameter, public :: crop_type_soybean = 2
  integer, parameter, public :: crop_type_maxval = 2

end module AgSysConstants
