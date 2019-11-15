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
    class(agsys_crop_type_generic), allocatable :: cultivar
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'ReadParams'
    !-----------------------------------------------------------------------

    SHR_ASSERT_FL((size(crops) == crop_type_maxval), sourcefile, __LINE__)

    allocate(agsys_crop_type_photosensitive :: crops(crop_type_maize)%cultivars(1))
    
    !!currently we hard-coded parameters for one maize cultivar (Pioneer_P04612XR_106)
    !!check the parameter values in the Maize.xml of APSIM
    !!http://apsrunet.apsim.info/websvn/filedetails.php?repname=apsim&path=%2Ftrunk%2FModel%2FMaize.xml
    !!TODO(pb, 2019-11-14) read parameters from files
    associate(cultivar => crops(crop_type_maize)%cultivars(1))
      ! can access params like this:
      ! cultivar_params%shoot_lag
      call cultivar%init()
      select type(cultivar)
        class is (agsys_crop_type_generic)
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

        class is (agsys_crop_type_photosensitive)
          cultivar%rc_photoperiod_target_tt%x=[0._r8, 12.5_r8, 20._r8]
          cultivar%rc_photoperiod_target_tt%y=[0._r8, 0._r8, 0._r8]
          cultivar%rc_photoperiod_target_tt%num_pts=3
        class is (agsys_crop_type_maize)
          cultivar%tt_emerg_to_endjuv = 275._r8
          cultivar%tt_flag_to_flower  = 1._r8
          cultivar%tt_flower_to_maturity = 812._r8
          cultivar%tt_flower_to_start_grain = 170._r8
          cultivar%tt_maturity_to_ripe = 1._r8
          cultivar%potential_kernel_weight = 300._r8
          cultivar%leaf_no_dead_const = -0.025_r8
          cultivar%leaf_no_dead_slope = 0.00035_r8      
      end select
    end associate

  end subroutine ReadParams

end module AgSysParamReader
