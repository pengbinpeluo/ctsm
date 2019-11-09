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

  ! !PUBLIC TYPES:

  type, public :: response_curve_type
     private

     ! Public data members
     integer, public :: num_pts
     real(r8), allocatable, public :: x(:)
     real(r8), allocatable, public :: y(:)
  end type response_curve_type

  ! Parameters that vary by crop
  type, public :: agsys_crop_params_type
     private

     ! Public data members
     integer, allocatable,      public :: MaxDaysFromSowingToEndofPhase(:)
     real(r8),                  public :: shoot_lag
     real(r8),                  public :: shoot_rate
     type(response_curve_type), public :: response_curve_tt
     type(response_curve_type), public :: response_curve_vd
     type(response_curve_type), public :: rc_sw_avail_phenol
     type(response_curve_type), public :: rc_sw_emerg_rate
     type(response_curve_type), public :: rc_photoperiod_TargetTT
     type(response_curve_type), public :: rc_cumvd_TargetTT
     real(r8),                  public :: p_photop_sens
     real(r8),                  public :: p_vern_sens
     real(r8),                  public :: p_pesw_germ
  end type agsys_crop_params_type

  ! Parameters that vary by cultivar
  type, public :: agsys_cultivar_params_type
     private

     ! Public data members
     real(r8), allocatable,     public :: phase_TargetTT(:)  !!target thermal time to finish a phase
     type(response_curve_type), public :: target_tt_from_photoperiod_end_of_juvenile
  end type agsys_cultivar_params_type

  ! Each crop has its own vector of cultivar-specific parameters. There is one instance
  ! of this derived type for each crop.
  type, public :: agsys_crop_cultivar_params_type
     private

     ! Public data members
     type(agsys_cultivar_params_type), allocatable, public :: cultivar_params(:)
  end type agsys_crop_cultivar_params_type

end module AgSysParams
