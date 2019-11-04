module AgSysParams

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Derived type holding AgSys's time-constant parameters, defining various crops and
  ! cultivars
  !
  ! Variables with suffix '_crop' are indexed by crop_type; variables with suffix
  ! '_cultivar' are indexed by cultivar.
  !
  ! !USES:
  use AgSysKinds, only : r8
  !
  implicit none
  private

  type, public :: response_curve_type
     private
     integer,  public :: cpnum  !!critical point number 
     real(r8), allocatable, public :: x(:)  !!critical point on x-axis
     real(r8), allocatable, public :: y(:)  !!critical point on y-axis
  end type response_curve_type

  ! !PUBLIC TYPES:
  type, public :: agsys_params_type
     private

     ! Public data members
     real(r8), allocatable, public :: shoot_lag_cultivar(:)
     real(r8), allocatable, public :: shoot_rate_cultivar(:)
  end type agsys_params_type

end module AgSysParams
