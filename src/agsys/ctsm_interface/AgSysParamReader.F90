module AgSysParamReader

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Routines for reading AgSys's time-constant parameters
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod,                 only : r8  => shr_kind_r8
  use shr_infnan_mod,               only : nan => shr_infnan_nan, assignment(=)
  use AgSysCropTypeGeneric,         only : agsys_cultivars_of_crop_type, agsys_crop_type_generic
  use AgSysCropTypePhotoSensitive,  only : agsys_crop_type_photosensitive
  use AgSysCropTypeMaize,           only : agsys_crop_type_maize
  use AgSysConstants,               only : crop_type_maxval, crop_type_maize
  !
  implicit none
  private

  ! !PUBLIC ROUTINES:
  public :: ReadParams

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine ReadParams(crops)
    !
    ! !DESCRIPTION:
    ! Read parameters
    !
    ! !ARGUMENTS:
    type(agsys_cultivars_of_crop_type), intent(inout) :: crops(:)
    !
    ! !LOCAL VARIABLES:
    class(agsys_crop_type_generic), allocatable :: cultivar
    integer :: crop_type

    character(len=*), parameter :: subname = 'ReadParams'
    !-----------------------------------------------------------------------

    SHR_ASSERT_FL((size(crops) == crop_type_maxval), sourcefile, __LINE__)

    allocate(agsys_crop_type_maize :: crops(crop_type_maize)%cultivars(1))
    
    !!currently we hard-coded parameters for one maize cultivar (Pioneer_P04612XR_106)
    !!check the parameter values in the Maize.xml of APSIM
    !!http://apsrunet.apsim.info/websvn/filedetails.php?repname=apsim&path=%2Ftrunk%2FModel%2FMaize.xml
    !!TODO(pb, 2019-11-14) read parameters from files
    associate(cultivar => crops(crop_type_maize)%cultivars(1))
      ! can access params like this:
      ! cultivar_params%shoot_lag
      call cultivar%init()
      allocate(cultivar%max_days_from_sowing_to_end_of_phase(11))
      cultivar%max_days_from_sowing_to_end_of_phase(:) = 0
      cultivar%p_sowing_depth = 50._r8  ! [mm], this is information about management and can be put
      ! into a separate structure later [TODO (2019-11-12, pb)]
      cultivar%p_shoot_lag    = 15._r8  ! [degree-days]
      cultivar%p_shoot_rate   = 0.6_r8  ! [degree-days per mm depth]
      cultivar%p_pesw_germ    = 0._r8   ! soil moisture threshold for seed germination

      cultivar%rc_tair_tt%x=[0._r8, 18._r8, 26._r8, 34._r8, 44._r8]
      cultivar%rc_tair_tt%y=[0._r8, 10._r8, 18._r8, 26._r8, 0._r8]
      cultivar%rc_tair_tt%num_pts=5

      cultivar%rc_sw_avail_phenol%x=[0._r8, 0.16_r8]
      cultivar%rc_sw_avail_phenol%y=[0._r8, 1._r8]
      cultivar%rc_sw_avail_phenol%num_pts=2

      cultivar%rc_sw_emerg_rate%x=[0._r8, 1._r8]
      cultivar%rc_sw_emerg_rate%y=[1._r8, 1._r8] !for maize, this is deactivated
      cultivar%rc_sw_emerg_rate%num_pts=2
      select type(cultivar)
      class is (agsys_crop_type_photosensitive)
         cultivar%rc_photoperiod_target_tt%x=[0._r8, 12.5_r8, 20._r8]
         cultivar%rc_photoperiod_target_tt%y=[0._r8, 0._r8, 0._r8]
         cultivar%rc_photoperiod_target_tt%num_pts=3
      end select
      select type(cultivar)
      class is (agsys_crop_type_maize)
         cultivar%target_tt_emerg_to_endjuv = 275._r8
         cultivar%target_tt_flag_to_flower  = 1._r8
         cultivar%target_tt_flower_to_maturity = 812._r8
         cultivar%target_tt_flower_to_start_grain = 170._r8
         cultivar%target_tt_maturity_to_ripe = 1._r8

         cultivar%leaf_init_rate = 23.2_r8
         cultivar%leaf_no_seed = 6._r8
         cultivar%leaf_no_at_emerg = 0.5_r8
         cultivar%leaf_no_min = 5.0_r8
         cultivar%leaf_no_max = 40._r8
         cultivar%leaf_no_critical = 11._r8
         cultivar%leaf_appearance_rate_early = 65._r8
         cultivar%leaf_appearance_rate_late = 36._r8

         cultivar%potential_kernel_weight = 300._r8
         cultivar%leaf_no_dead_const = -0.025_r8
         cultivar%leaf_no_dead_slope = 0.00035_r8      
      end select
      end associate

      ! TODO(wjs, 2019-11-15) Set other crops / cultivars. For now just initialize to
      ! default values.
      do crop_type = 1, crop_type_maxval
         if (crop_type /= crop_type_maize) then
            allocate(agsys_crop_type_generic :: crops(crop_type)%cultivars(1))
            call crops(crop_type)%cultivars(1)%init()
         end if
      end do

  end subroutine ReadParams

end module AgSysParamReader
