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
  use AgSysCommon, only : agsys_common_type
  use AgSysParams, only : agsys_params_type
  use AgSysClimate, only : agsys_climate_type
  use AgSysPhenology, only : agsys_phenology_type
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_type
     private
     type(agsys_common_type)    :: agsys_common_inst
     type(agsys_params_type)    :: agsys_params_inst
     type(agsys_climate_type)   :: agsys_climate_inst
     type(agsys_phenology_type) :: agsys_phenology_inst

   contains
     procedure, public :: AgSysDriver
     procedure, public :: Init

  end type agsys_type

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
    class(agsys_type), intent(inout) :: this
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'AgSysDriver'
    !-----------------------------------------------------------------------

    call this%agsys_climate_inst%AgSysClimateTimeStep()

    if (is_beg_curr_day()) then
       call this%agsys_phenology_inst%AgSysPhenologyTimeStep()
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
    class(agsys_type), intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    call this%agsys_common_inst%Init(bounds)
    call this%agsys_params_inst%Init()
    call this%agsys_climate_inst%Init(bounds)
    call this%agsys_phenology_inst%Init(bounds)

  end subroutine Init

end module AgSys
