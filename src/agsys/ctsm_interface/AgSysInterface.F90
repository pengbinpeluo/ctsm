module AgSysInterface

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Top-level type and control for the AgSys model
  !
  ! This is a model of crop phenology and allocation, based on the APSIM model
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod    , only : r8 => shr_kind_r8
  use clm_time_manager, only : is_beg_curr_day
  use decompMod, only : bounds_type
  use clm_varpar, only : nlevsoi
  use PatchType, only : patch_type
  use GridcellType, only : gridcell_type
  use TemperatureType, only : temperature_type
  use AgSysConstants, only : crop_type_maxval, crop_type_not_handled
  use AgSysType, only : agsys_type
  use AgSysParams, only : agsys_crop_cultivar_params_type
  use AgSysPhases, only : agsys_phases_type
  use AgSysParamReader, only : ReadParams, ReadPhases
  use AgSysRuntimeConstants, only : InitRuntimeConstants
  use AgSysPlaceholder, only : DoTimeStep_Phenology_Placeholder, AgsysAbioticStress_Placeholder
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_interface_type
     private
     ! Parameters that vary by cultivar; these are first indexed by crop type, then
     ! indexed by the specific cultivar for that crop. For example:
     !
     ! do p = ...  <- loop over patches (typically a filter loop)
     !    crop_type = agsys_general_inst%crop_type_patch(p)
     !    cultivar  = agsys_general_inst%cultivar_patch(p)
     !    call SomeAgsysRoutine( &
     !         cultivar_params = agsys_inst%crop_cultivar_params(crop_type)%cultivar_params(cultivar), &
     !         ...)
     ! end do
     !
     ! (Note that this way, the science code - such as SomeAgsysRoutine - doesn't need to
     ! index the params by crop type or cultivar: a given call for a single patch only
     ! has access to the parameters for the appropriate crop type and cultivar for that
     ! patch.)
     type(agsys_crop_cultivar_params_type) :: crop_cultivar_params(crop_type_maxval)

     ! Information about the phases for each crop type
     type(agsys_phases_type) :: crop_phases(crop_type_maxval)

     type(agsys_type) :: agsys_inst

   contains
     procedure, public :: AgSysDriver
     procedure, public :: Init
  end type agsys_interface_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  ! ========================================================================
  ! Science routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine AgSysDriver(this, num_pcropp, filter_pcropp, &
       patch, grc, temperature_inst)
    !
    ! !DESCRIPTION:
    ! Coordinate the work of AgSys
    !
    ! This should be called every time step, even though some parts of AgSys are done
    ! less frequently (e.g., daily updates of phenology).
    !
    ! !ARGUMENTS:
    class(agsys_interface_type), intent(inout) :: this
    integer                    , intent(in)    :: num_pcropp       ! number of prog crop patches in filter
    integer                    , intent(in)    :: filter_pcropp(:) ! filter for prognostic crop patches
    type(patch_type)           , intent(in)    :: patch
    type(gridcell_type)        , intent(in)    :: grc
    type(temperature_type)     , intent(in)    :: temperature_inst
    !
    ! !LOCAL VARIABLES:
    integer :: fp, p, g, c
    integer :: crop_type  ! AgSys's crop type for this patch
    integer :: cultivar   ! AgSys's cultivar type for this patch
    real(r8) :: sw_avail_ratio
    real(r8) :: pesw_seedlayer

    character(len=*), parameter :: subname = 'AgSysDriver'
    !-----------------------------------------------------------------------

    ! TODO(wjs, 2019-11-08) Do climate stuff. This will involve updating AgSys-specific accumulator fields.

    ! Note that 24-hour temperature accumulators (which are needed here) are updated late
    ! in the driver loop, based on end_curr_day. We use beg_curr_day here so that the
    ! phenology routines are called the time step after these temperature accumulators
    ! are updated. (If we used end_curr_day here, we wouldn't have today's updated
    ! temperature accumulators, because the current routine is called before the various
    ! UpdateAccVars calls in the driver loop.)
    if (is_beg_curr_day()) then
       do fp = 1, num_pcropp
          p = filter_pcropp(fp)

          crop_type = this%agsys_inst%crop_type_patch(p)
          if (crop_type /= crop_type_not_handled) then
             g = patch%gridcell(p)
             c = patch%column(p)
             cultivar = this%agsys_inst%cultivar_patch(p)

             call AgsysAbioticStress_Placeholder( &
                  ! Inputs, time-varying
                  h2osoi_liq_24hr = this%agsys_inst%h2osoi_liq_24hr_col(c, 1:nlevsoi), &

                  ! Outputs
                  sw_avail_ratio = sw_avail_ratio, &
                  pesw_seedlayer = pesw_seedlayer)

             call DoTimeStep_Phenology_Placeholder( &
                  ! Inputs, time-constant
                  croptype        = crop_type, &
                  phases          = this%crop_phases(crop_type), &
                  cultivar_params = this%crop_cultivar_params(crop_type)%cultivar_params(cultivar), &

                  ! Inputs, time-varying
                  photoperiod    = grc%dayl(g), &
                  tair_max       = temperature_inst%t_ref2m_max_patch(p), &
                  tair_min       = temperature_inst%t_ref2m_min_patch(p), &
                  tc             = temperature_inst%t_veg24_patch(p), &
                  sw_avail_ratio = sw_avail_ratio, &
                  pesw_seedlayer = pesw_seedlayer, &

                  ! Outputs
                  days_after_sowing = this%agsys_inst%days_after_sowing_patch(p), &
                  current_stage     = this%agsys_inst%current_stage_patch(p), &
                  days_in_phase     = this%agsys_inst%days_in_phase_patch(:,p), &
                  tt_in_phase       = this%agsys_inst%acc_thermal_time_in_phase_patch(:,p), &
                  days_after_phase  = this%agsys_inst%days_after_phase_patch(:,p), &
                  tt_after_phase    = this%agsys_inst%acc_thermal_time_after_phase_patch(:,p), &
                  cumvd             = this%agsys_inst%acc_vernalization_days_patch(p))
          end if
       end do
    end if

  end subroutine AgSysDriver

  ! ========================================================================
  ! Infrastructure routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds, patch)
    !
    ! !DESCRIPTION:
    ! Initialize variables needed by the AgSys model
    !
    ! !ARGUMENTS:
    class(agsys_interface_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    type(patch_type), intent(in) :: patch
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    call ReadParams(this%crop_cultivar_params)
    call ReadPhases(this%crop_phases)
    call InitRuntimeConstants(this%crop_phases)
    call this%agsys_inst%Init(bounds, patch)

  end subroutine Init

end module AgSysInterface
