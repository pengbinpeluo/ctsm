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
  use AgSysPhases      , only : agsys_phases_type, &
                                max_str_len_for_phase_def, &
                                phase_type_generic, phase_type_germinating, phase_type_emerging, &
                                phase_type_leaf_appearance
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
      ! cultivar_params%shoot_lag
      cultivar_params%shoot_lag = 15._r8  ! degree-days
      cultivar_params%shoot_rate= 0.6_r8  ! degree-days per mm depth

      cultivar_params%response_curve_tt%x=[0._r8, 18._r8, 26._r8, 34._r8, 44._r8]
      cultivar_params%response_curve_tt%y=[0._r8, 10._r8, 18._r8, 26._r8, 0._r8]
      cultivar_params%response_curve_tt%num_pts=5

      cultivar_params%rc_sw_avail_phenol%x=[0._r8, 0.16_r8]
      cultivar_params%rc_sw_avail_phenol%y=[0._r8, 1._r8]
      cultivar_params%rc_sw_avail_phenol%num_pts=2
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
    
    crop_phases(crop_type_maize)%num_phases=11
    crop_phases(crop_type_maize)%stage_name=[character(len=max_str_len_for_phase_def) :: &
                                              'sowing', 'germination', 'emergence', 'end_of_juvenile', &
                                              'floral_initiation', 'flag_leaf', 'flowering', &
                                              'start_grain_fill', 'end_grain_fill', 'maturity', 'harvest_ripe', 'end_crop']
    crop_phases(crop_type_maize)%phase_name=[character(len=max_str_len_for_phase_def) :: &
                                              'germinating', 'emerging', 'juvenile', &
                                              'photosen_sitive', 'leaf_apperance', 'flag_leaf_to_flowering', &
                                              'flowering_to_grain_filling', 'grain_filling', 'maturing', 'maturity_to_harvest_ripe', 'ready_for_harvesting']
    crop_phases(crop_type_maize)%phase_type=[phase_type_germinating, phase_type_emerging, phase_type_generic, &
                                             phase_type_generic, phase_type_generic, phase_type_generic, &
                                             phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic]

  end subroutine ReadPhases

end module AgSysParamReader
