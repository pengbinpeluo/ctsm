module AgSysParamReader

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Routines for reading AgSys's time-constant parameters
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_infnan_mod   , only : nan => shr_infnan_nan, assignment(=)
  use AgSysParams      , only : agsys_params_type
  use AgSysPhases      , only : agsys_phases_type
  !
  implicit none
  private

  ! !PUBLIC ROUTINES:
  public :: ReadParams
  public :: ReadPhases

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine ReadParams(params)
    !
    ! !DESCRIPTION:
    ! Read parameters
    !
    ! !ARGUMENTS:
    type(agsys_params_type), intent(inout) :: params
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'ReadParams'
    !-----------------------------------------------------------------------

  end subroutine ReadParams

  !-----------------------------------------------------------------------
  subroutine ReadPhases(phases)
    !
    ! !DESCRIPTION:
    ! Read phase descriptions for each crop
    !
    ! !ARGUMENTS:
    type(agsys_phases_type), intent(inout) :: phases
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'ReadPhases'
    !-----------------------------------------------------------------------

  end subroutine ReadPhases

end module AgSysParamReader
