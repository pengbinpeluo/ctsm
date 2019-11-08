module AgSys

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Top-level type and control for the AgSys model
  !
  ! This is a model of crop phenology and allocation, based on the APSIM model
  !
  ! !USES:
#include "shr_assert.h"
  use clm_time_manager, only : is_beg_curr_day
  use decompMod, only : bounds_type
  use AgSysConstants, only : crop_type_maxval
  use AgSysType, only : agsys_type
  use AgSysParams, only : agsys_crop_params_type, agsys_crop_cultivar_params_type
  use AgSysPhases, only : agsys_phases_type
  use AgSysParamReader, only : ReadParams, ReadPhases
  use AgSysRuntimeConstants, only : InitRuntimeConstants
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_interface_type
     private
     ! Parameters that vary by crop type
     type(agsys_crop_params_type) :: crop_params(crop_type_maxval)

     ! Parameters that vary by cultivar; these are first indexed by crop type, then
     ! indexed by the specific cultivar for that crop. For example:
     !
     ! do p = ...  <- loop over patches (typically a filter loop)
     !    crop_type = agsys_general_inst%crop_type_patch(p)
     !    cultivar  = agsys_general_inst%cultivar_patch(p)
     !    call SomeAgsysRoutine( &
     !         crop_params     = agsys_inst%crop_params(crop_type), &
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
  subroutine AgSysDriver(this)
    !
    ! !DESCRIPTION:
    ! Coordinate the work of AgSys
    !
    ! This should be called every time step, even though some parts of AgSys are done
    ! less frequently (e.g., daily updates of phenology).
    !
    ! !ARGUMENTS:
    class(agsys_interface_type), intent(inout) :: this
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'AgSysDriver'
    !-----------------------------------------------------------------------

    ! TODO(wjs, 2019-11-08) Do climate stuff

    if (is_beg_curr_day()) then
       ! TODO(wjs, 2019-11-08) Do phenology stuff
    end if

  end subroutine AgSysDriver

  ! ========================================================================
  ! Infrastructure routines
  ! ========================================================================

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize variables needed by the AgSys model
    !
    ! !ARGUMENTS:
    class(agsys_interface_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    call ReadParams(this%crop_params, this%crop_cultivar_params)
    call ReadPhases(this%crop_phases)
    call InitRuntimeConstants(this%crop_phases)
    call this%agsys_inst%Init(bounds)

  end subroutine Init

end module AgSys
