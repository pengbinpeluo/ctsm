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
  use AgSysRuntimeConstants, only : agsys_max_phases
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
     ! how far we are from one stage to the next.
     real(r8), pointer, public :: current_stage_patch(:)

     ! Whether the crop has emerged yet this season
     logical, pointer, public :: emerged_patch(:)

     ! Accumulated thermal time (deg-days)
     real(r8), pointer, public :: acc_thermal_time_patch(:)

     ! TODO(wjs, 2019-11-01) We may not need all of these - i.e., it maybe unnecessary to
     ! have both an emerged_thermal_time and thermal time for each phase (since the former
     ! can be calculated from the latter if needed). Also, we may not actually need the
     ! full generality of having thermal time for each phase: it may be sufficient, for
     ! example, to store the thermal time just for the previous phase. (If we can avoid
     ! supporting this full generality, that could be good to avoid restart file bloat.)
     real(r8), pointer, public :: acc_emerged_thermal_time_patch(:)  ! accumulated thermal time since emergence (deg-days)
     real(r8), pointer, public :: acc_thermal_time_phases_patch(:,:) ! accumulated thermal time for each phase (deg-days) [phase, patch]

     integer, pointer, public :: days_after_sowing_patch(:)
     real(r8), pointer, public :: prop_of_day_to_use_patch(:)


   contains
     procedure, public :: Init
     procedure, private :: InitAllocate
  end type agsys_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_type instance
    !
    ! !ARGUMENTS:
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    call this%InitAllocate(bounds)
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
         endp => bounds%endp  &
         )

    allocate(this%crop_type_patch(begp:endp)); this%crop_type_patch(:) = 0
    allocate(this%cultivar_patch(begp:endp)); this%cultivar_patch(:) = 0
    allocate(this%current_stage_patch(begp:endp)); this%current_stage_patch(:) = nan
    allocate(this%emerged_patch(begp:endp)); this%emerged_patch(:) = .false.

    allocate(this%acc_thermal_time_patch(begp:endp))         ; this%acc_thermal_time_patch(:) = nan
    allocate(this%acc_emerged_thermal_time_patch(begp:endp)) ; this%acc_emerged_thermal_time_patch(:) = nan

    allocate(this%acc_thermal_time_phases_patch(1:agsys_max_phases, begp:endp))
    this%acc_thermal_time_phases_patch(:,:) = nan

    allocate(this%days_after_sowing_patch(begp:endp))  ; this%days_after_sowing_patch(:) = 0
    allocate(this%prop_of_day_to_use_patch(begp:endp)) ; this%prop_of_day_to_use_patch(:) = nan

    end associate
  end subroutine InitAllocate

end module AgSysType
