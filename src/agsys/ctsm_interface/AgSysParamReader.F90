module AgSysParamReader

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Routines for reading AgSys's time-constant parameters
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_infnan_mod   , only : nan => shr_infnan_nan, assignment(=)
  use AgSysParams      , only : agsys_crop_cultivar_params_type
  use AgSysPhases      , only : agsys_phases_type
  use AgSysConstants   , only : crop_type_maxval, crop_type_maize
  !
  implicit none
  private

  ! !PUBLIC ROUTINES:
  public :: ReadParams
  public :: ReadPhases

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine ReadParams(crop_cultivar_params)
    !
    ! !DESCRIPTION:
    ! Read parameters
    !
    ! !ARGUMENTS:
    type(agsys_crop_cultivar_params_type), intent(inout) :: crop_cultivar_params(:)
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'ReadParams'
    !-----------------------------------------------------------------------

    SHR_ASSERT_FL((size(crop_cultivar_params) == crop_type_maxval), sourcefile, __LINE__)

    allocate(crop_cultivar_params(crop_type_maize)%cultivar_params(1))

    associate(cultivar_params => crop_cultivar_params(crop_type_maize)%cultivar_params(1))

    ! can access params like this:
    ! cultivar_params%target_tt_from_photoperiod_end_of_juvenile

    end associate

  end subroutine ReadParams

  !-----------------------------------------------------------------------
  subroutine ReadPhases(crop_phases)
    !
    ! !DESCRIPTION:
    ! Read phase descriptions for each crop
    !
    ! !ARGUMENTS:
    type(agsys_phases_type), intent(inout) :: crop_phases(:)
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'ReadPhases'
    !-----------------------------------------------------------------------

    SHR_ASSERT_FL((size(crop_phases) == crop_type_maxval), sourcefile, __LINE__)

  end subroutine ReadPhases

end module AgSysParamReader
