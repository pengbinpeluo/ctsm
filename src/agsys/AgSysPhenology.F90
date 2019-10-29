module AgSysPhenology

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class for storing and calculating AgSys's phenology-related variables
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

  type, public :: agsys_phenology_type
     private

     ! Private data members
     integer, pointer :: days_after_sowing_patch(:)
     real(r8), pointer :: prop_of_day_to_use_patch(:)

   contains
     procedure, public :: AgSysPhenologyTimeStep

     procedure, public :: Init
     procedure, private :: InitAllocate
  end type agsys_phenology_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  ! ========================================================================
  ! Science routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine AgSysPhenologyTimeStep(this)
    !
    ! !DESCRIPTION:
    ! Update phenology variables each phenology time step
    !
    ! This should be called once per day (which is the length of a phenology time step)
    !
    ! !ARGUMENTS:
    class(agsys_phenology_type), intent(inout) :: this
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'AgSysPhenologyTimeStep'
    !-----------------------------------------------------------------------

  end subroutine AgSysPhenologyTimeStep

  ! ========================================================================
  ! Infrastructure routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_phenology_type insntance
    !
    ! !ARGUMENTS:
    class(agsys_phenology_type), intent(inout) :: this
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
    ! Allocate components of this agsys_phenology_type instance
    !
    ! !ARGUMENTS:
    class(agsys_phenology_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'InitAllocate'
    !-----------------------------------------------------------------------

    associate( &
         begp => bounds%begp, &
         endp => bounds%endp  &
         )

    allocate(this%days_after_sowing_patch(begp:endp))  ; this%days_after_sowing_patch(:) = 0
    allocate(this%prop_of_day_to_use_patch(begp:endp)) ; this%prop_of_day_to_use_patch(:) = nan

    end associate

  end subroutine InitAllocate

end module AgSysPhenology
