module AgSysClimateInterface

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class for storing and calculating climate-related inputs to AgSys
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

  type, public :: agsys_climate_type
     private

     ! Public data members
     real(r8), pointer, public :: accumulated_thermal_time_patch(:)          ! accumulated thermal time (deg-days)

     ! TODO(wjs, 2019-11-01) We may not need all of these - i.e., it maybe unnecessary to
     ! have both an emerged_thermal_time and thermal time for each phase (since the former
     ! can be calculated from the latter if needed). Also, we may not actually need the
     ! full generality of having thermal time for each phase: it may be sufficient, for
     ! example, to store the thermal time just for the previous phase. (If we can avoid
     ! supporting this full generality, that could be good to avoid restart file bloat.)
     real(r8), pointer, public :: accumulated_emerged_thermal_time_patch(:)  ! accumulated thermal time since emergence (deg-days)
     real(r8), pointer, public :: accumulated_thermal_time_phases_patch(:,:) ! accumulated thermal time for each phase (deg-days) [phase, patch]
   contains
     procedure, public :: AgSysClimateTimeStep

     procedure, public :: Init
     procedure, private :: InitAllocate
  end type agsys_climate_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  ! ========================================================================
  ! Science routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine AgSysClimateTimeStep(this)
    !
    ! !DESCRIPTION:
    ! Update climate variables each time step
    !
    ! This routine should be called every model time step
    !
    ! !ARGUMENTS:
    class(agsys_climate_type), intent(inout) :: this
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'AgSysClimateTimeStep'
    !-----------------------------------------------------------------------

  end subroutine AgSysClimateTimeStep


  ! ========================================================================
  ! Infrastructure routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_climate_type instance
    !
    ! !ARGUMENTS:
    class(agsys_climate_type), intent(inout) :: this
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
    ! Allocate components of this agsys_climate_type instance
    !
    ! !ARGUMENTS:
    class(agsys_climate_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'InitAllocate'
    !-----------------------------------------------------------------------

    associate( &
         begp => bounds%begp, &
         endp => bounds%endp  &
         )

    allocate(this%accumulated_thermal_time_patch(begp:endp))         ; this%accumulated_thermal_time_patch(:) = nan
    allocate(this%accumulated_emerged_thermal_time_patch(begp:endp)) ; this%accumulated_emerged_thermal_time_patch(:) = nan

    allocate(this%accumulated_thermal_time_phases_patch(1:agsys_max_phases, begp:endp))
    this%accumulated_thermal_time_phases_patch(:,:) = nan

    end associate

  end subroutine InitAllocate

end module AgSysClimateInterface
