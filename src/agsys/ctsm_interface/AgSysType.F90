module AgSysType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class containing data used by AgSys
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_infnan_mod   , only : nan => shr_infnan_nan, assignment(=)
  use decompMod        , only : bounds_type
  use clm_varpar       , only : nlevsoi
  use clm_varcon       , only : spval
  use pftconMod        , only : ntmp_corn, nirrig_tmp_corn, ntrp_corn, nirrig_trp_corn
  use histFileMod      , only : hist_addfld1d, hist_addfld2d
  use PatchType        , only : patch_type
  use AgSysRuntimeConstants,    only : agsys_max_phases
  use AgSysConstants,           only : crop_type_not_handled, crop_type_maize
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysRoot,                only : agsys_soil_property_type, agsys_soil_condition_type, agsys_root_type     
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_type
     private

     ! ------------------------------------------------------------------------
     ! Public data members, time-constant
     ! ------------------------------------------------------------------------

     ! AgSys's crop type: one of the crop_type_* constants defined in AgSysConstants; note
     ! that these may differ from the constants in pftconMod
     integer, pointer, public :: crop_type_patch(:)

     ! Cultivar type. For a given crop type, the cultivar type numbering starts at 1. So
     ! maize may have cultivars 1-3, soybean 1-2, wheat 1-4, etc. Each crop type has at
     ! least one cultivar, and may have many. This is currently constant in time, but
     ! eventually may be dynamic.
     integer, pointer, public :: cultivar_patch(:)

     ! ------------------------------------------------------------------------
     ! Public data members, time-varying
     ! ------------------------------------------------------------------------

     ! Current stage, as an integer that counts up in order from 1 to the maximum number
     ! of stages for the given crop type. This can have a fractional value, indicating
     ! how far we are from one stage to the next. Before sowing, this is 0.
     real(r8), pointer, public :: current_stage_patch(:)

     ! Whether the crop is currently alive; true in the entire period from sowing to harvest
     logical, pointer, public :: crop_alive_patch(:)

     ! Whether the crop has emerged yet this season
     logical, pointer, public :: emerged_patch(:)

     ! TODO(wjs, 2019-11-13) 2-d variables are stored with patch (or column) as the first
     ! dimension. This follows CTSM conventions, but is less efficient for AgSys, where we
     ! operate on a single point at a time. Ideally, for the sake of performance, we would
     ! switch this dimension order for 2-d AgSys variables. However, currently that
     ! prevents us from doing history output for the given 2-d variable. (Also, although
     ! the restart routines seem to be general enough to handle variables with either
     ! dimension ordering - via switchdim - it doesn't look like there are any variables
     ! that currently have switchdim = .false., so some changes may be needed to support
     ! that robustly, in the restart utilities and/or in init_interp.) At some point, we
     ! should consider putting in place the necessary generalization for the history
     ! infrastructure to handle 2-d variables with this switched dimension ordering (as
     ! long as this can be done in a way that doesn't hurt the efficiency of the history
     ! infrastructure). At that point, we should change the variables here so that patch
     ! (or column) is the second dimension.

     real(r8), pointer, public :: days_in_phase_patch(:,:)  ! number of days in each phase [patch, phase]
     real(r8), pointer, public :: days_after_phase_patch(:,:)  ! number of days after each phase [patch, phase]

     ! TODO(wjs, 2019-11-01) We may not need all of these - i.e., it maybe unnecessary to
     ! have both an emerged_thermal_time and thermal time for each phase (since the former
     ! can be calculated from the latter if needed). Also, we may not actually need the
     ! full generality of having thermal time for each phase: it may be sufficient, for
     ! example, to store the thermal time just for the previous phase. (If we can avoid
     ! supporting this full generality, that could be good to avoid restart file bloat.)
     real(r8), pointer, public :: acc_emerged_thermal_time_patch(:)  ! accumulated thermal time since emergence (deg-days)
     real(r8), pointer, public :: acc_thermal_time_in_phase_patch(:,:) ! accumulated thermal time in each phase (deg-days) [patch, phase]
     real(r8), pointer, public :: acc_thermal_time_after_phase_patch(:,:) ! accumulated thermal time after each phase (deg-days) [patch, phase]
     real(r8), pointer, public :: phase_target_thermal_time_patch(:,:)  !target thermal time to finish a phase (deg-days) [path, phase]
     real(r8), pointer, public :: acc_vernalization_days_patch(:) ! accumulated vernalization days (for crops with vernalization) (unit: days)

     real(r8), pointer, public :: h2osoi_liq_24hr_col(:,:)  ! 24-hour average h2osoi_liq (kg/m2), just over 1:nlevsoi

     integer, pointer, public :: days_after_sowing_patch(:)

     ! We store an instance of this so that we only need to allocate memory for it once,
     ! in initialization
     type(agsys_environmental_inputs_type), public :: agsys_environmental_inputs
     type(agsys_soil_property_type),  public :: agsys_soil_properties
     type(agsys_soil_condition_type), public :: agsys_soil_condition
     type(agsys_root_type),           public :: agsys_root_properties
   contains
     procedure, public :: Init
     procedure, private :: InitAllocate
     procedure, private :: InitHistory
     procedure, private :: InitCold
  end type agsys_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds, patch)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_type instance
    !
    ! !ARGUMENTS:
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    type(patch_type), intent(in) :: patch
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    call this%InitAllocate(bounds)
    call this%InitHistory(bounds)
    call this%InitCold(bounds, patch)
  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! !DESCRIPTION:
    ! Allocate components of this agsys_type instance
    !
    ! !ARGUMENTS:
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'InitAllocate'
    !-----------------------------------------------------------------------

    associate( &
         begp => bounds%begp, &
         endp => bounds%endp, &
         begc => bounds%begc, &
         endc => bounds%endc  &
         )

    allocate(this%crop_type_patch(begp:endp)); this%crop_type_patch(:) = 0
    allocate(this%cultivar_patch(begp:endp)); this%cultivar_patch(:) = 0
    allocate(this%current_stage_patch(begp:endp)); this%current_stage_patch(:) = nan
    allocate(this%crop_alive_patch(begp:endp)); this%crop_alive_patch(:) = .false.
    allocate(this%emerged_patch(begp:endp)); this%emerged_patch(:) = .false.

    allocate(this%days_in_phase_patch(begp:endp, 1:agsys_max_phases))
    this%days_in_phase_patch(:,:) = nan
    allocate(this%days_after_phase_patch(begp:endp, 1:agsys_max_phases))
    this%days_after_phase_patch(:,:) = nan

    allocate(this%acc_emerged_thermal_time_patch(begp:endp)) ; this%acc_emerged_thermal_time_patch(:) = nan
    allocate(this%acc_thermal_time_in_phase_patch(begp:endp, 1:agsys_max_phases))
    this%acc_thermal_time_in_phase_patch(:,:) = nan
    allocate(this%acc_thermal_time_after_phase_patch(begp:endp, 1:agsys_max_phases))
    this%acc_thermal_time_after_phase_patch(:,:) = nan

    allocate(this%acc_vernalization_days_patch(begp:endp)); this%acc_vernalization_days_patch(:) = nan

    allocate(this%h2osoi_liq_24hr_col(begc:endc, 1:nlevsoi)); this%h2osoi_liq_24hr_col(:,:) = nan

    allocate(this%days_after_sowing_patch(begp:endp))  ; this%days_after_sowing_patch(:) = 0

    call this%agsys_environmental_inputs%Init( &
         nlevsoi = nlevsoi)

    end associate
  end subroutine InitAllocate

  !-----------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize history fields for this agsys instance
    !
    ! !ARGUMENTS:
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'InitHistory'
    !-----------------------------------------------------------------------

    associate( &
         begp => bounds%begp, &
         endp => bounds%endp  &
         )

    this%current_stage_patch(begp:endp) = spval
    call hist_addfld1d(fname='AGSYS_CURRENT_STAGE', units='-', &
         avgflag='I', long_name='Current phenological stage number (at end of history period)', &
         ptr_patch=this%current_stage_patch)

    this%acc_thermal_time_in_phase_patch(begp:endp,:) = spval
    call hist_addfld2d(fname='AGSYS_ACC_THERMAL_TIME_IN_PHASE', units='deg-days', &
         type2d='agsys_phases', &
         avgflag='I', long_name='Accumulated thermal time in each phase (at end of history period)', &
         ptr_patch=this%acc_thermal_time_in_phase_patch)

    end associate

  end subroutine InitHistory

  !-----------------------------------------------------------------------
  subroutine InitCold(this, bounds, patch)
    !
    ! !DESCRIPTION:
    ! Do cold start initialization for this agsys instance
    !
    ! !ARGUMENTS:
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    type(patch_type), intent(in) :: patch
    !
    ! !LOCAL VARIABLES:
    integer :: p

    character(len=*), parameter :: subname = 'InitCold'
    !-----------------------------------------------------------------------

    associate( &
         begp => bounds%begp, &
         endp => bounds%endp, &
         begc => bounds%begc, &
         endc => bounds%endc  &
         )

    do p = begp, endp
       associate( &
            ptype => patch%itype(p) &
            )

       if ( ptype == ntmp_corn .or. ptype == nirrig_tmp_corn .or. &
            ptype == ntrp_corn .or. ptype == nirrig_trp_corn) then
          this%crop_type_patch(p) = crop_type_maize
          ! TODO(wjs, 2019-11-12) Handle more crop types here
       else
          this%crop_type_patch(p) = crop_type_not_handled
       end if

       ! TODO(wjs, 2019-11-12) Fix this
       this%cultivar_patch(p) = 1

       end associate
    end do

    this%current_stage_patch(begp:endp) = 0._r8
    this%crop_alive_patch(begp:endp) = .false.
    this%emerged_patch(begp:endp) = .false.

    this%days_in_phase_patch(begp:endp, :) = 0._r8
    this%days_after_phase_patch(begp:endp, :) = 0._r8

    this%acc_emerged_thermal_time_patch(begp:endp) = 0._r8
    this%acc_thermal_time_in_phase_patch(begp:endp, :) = 0._r8
    this%acc_thermal_time_after_phase_patch(begp:endp, :) = 0._r8
    this%acc_vernalization_days_patch(:) = 0._r8

    ! TODO(wjs, 2019-11-12) We may be able to remove this initialization once we properly
    ! initialize the accumulator field related to this variable
    this%h2osoi_liq_24hr_col(begc:endc, :) = 0._r8

    this%days_after_sowing_patch(begp:endp) = 0

    end associate

  end subroutine InitCold

end module AgSysType
