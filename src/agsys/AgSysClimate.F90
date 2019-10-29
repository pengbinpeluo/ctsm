module AgSysClimate

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class for storing and calculating climate-related inputs to AgSys
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_infnan_mod   , only : nan => shr_infnan_nan, assignment(=)
  use decompMod        , only : bounds_type
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_climate_type
     private

     ! Public data members
     real(r8), pointer, public :: accumulated_thermal_time_patch(:)        ! accumulated thermal time (deg-days)
     real(r8), pointer, public :: accumulated_emerged_thermal_time_patch(:) ! accumulated thermal time since emergence (deg-days)
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
    ! Initialize this agsys_climate_type insntance
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

    end associate

  end subroutine InitAllocate

end module AgSysClimate
