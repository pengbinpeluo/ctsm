module AgSysParams

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Class holding AgSys's time-constant parameters, defining various crops and cultivars
  !
  ! Variables with suffix '_crop' are indexed by crop_type; variables with suffix
  ! '_cultivar' are indexed by cultivar.
  !
  ! !USES:
#include "shr_assert.h"
  use shr_kind_mod     , only : r8 => shr_kind_r8
  use shr_infnan_mod   , only : nan => shr_infnan_nan, assignment(=)
  !
  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_params_type
     private

     ! Public data members
     real(r8), allocatable :: shoot_lag_cultivar(:)
     real(r8), allocatable :: shoot_rate_cultivar(:)
   contains
     procedure, public :: Init
  end type agsys_params_type

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine Init(this)
    !
    ! !DESCRIPTION:
    ! Initialize this agsys_params_type instance
    !
    ! !ARGUMENTS:
    class(agsys_params_type), intent(inout) :: this
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

  end subroutine Init

end module AgSysParams
