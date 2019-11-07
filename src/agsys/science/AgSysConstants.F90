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
  integer, parameter, public :: crop_type_wheat = 3
  integer, parameter, public :: crop_type_sorghum = 4
  integer, parameter, public :: crop_type_maxval = 4

end module AgSysConstants
