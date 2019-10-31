module AgSysGeneral

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class containing general data used by multiple other AgSys classes
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

  type, public :: agsys_general_type
     private

     ! ------------------------------------------------------------------------
     ! Public data members, time-constant
     ! ------------------------------------------------------------------------

     ! AgSys's crop type: one of the crop_type_* constants defined in AgSysConstants; note
     ! that these may differ from the constants in pftconMod
     integer, pointer, public :: crop_type_patch(:)

     ! Cultivar type. A given value implies a given crop type; for example, cultivar
     ! values 1-3 may always be maize cultivars, and cultivar values 4-5 always soybean
     ! cultivars, etc. - so you can index cultivar-specific parameters just with
     ! cultivar_patch, without needing to also reference crop_type_patch. Each crop type
     ! has at least one cultivar, and may have many. This is currently constant in time,
     ! but eventually may be dynamic.
     integer, pointer, public :: cultivar_patch(:)

     ! ------------------------------------------------------------------------
     ! Public data members, time-varying
     ! ------------------------------------------------------------------------

     ! Current stage, as an integer that counts up in order from 1 to the maximum number
     ! of stages for the given crop type.
     integer, pointer, public :: current_stage_patch(:)

     ! Whether the crop has emerged yet this season
     logical, pointer, public :: emerged_patch(:)

   contains
     procedure, public :: Init
     procedure, private :: InitAllocate
  end type agsys_general_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_general_type instance
    !
    ! !ARGUMENTS:
    class(agsys_general_type), intent(inout) :: this
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
    ! Allocate components of this agsys_general_type instance
    !
    ! !ARGUMENTS:
    class(agsys_general_type), intent(inout) :: this
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
    allocate(this%current_stage_patch(begp:endp)); this%current_stage_patch(:) = 0
    allocate(this%emerged_patch(begp:endp)); this%emerged_patch(:) = .false.

    end associate
  end subroutine InitAllocate

end module AgSysGeneral
