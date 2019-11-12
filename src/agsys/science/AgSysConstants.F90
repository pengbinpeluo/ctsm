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
  !
  ! NOTE(wjs, 2019-11-12) When changing these constants or adding new crop types, also
  ! change the corresponding unit test in
  ! src/agsys/test/AgSys_Enumerations_test/test_enumerations.pf.
  integer, parameter, public :: crop_type_minval = 0
  integer, parameter, public :: crop_type_not_handled = 0  ! For crop types not handled by AgSys
  integer, parameter, public :: crop_type_maize = 1
  integer, parameter, public :: crop_type_soybean = 2
  integer, parameter, public :: crop_type_wheat = 3
  integer, parameter, public :: crop_type_sorghum = 4
  integer, parameter, public :: crop_type_maxval = 4

end module AgSysConstants
