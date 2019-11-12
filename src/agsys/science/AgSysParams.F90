module AgSysParams

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Derived type holding AgSys's time-constant parameters, defining various crops and
  ! cultivars
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

  ! Parameters that vary by cultivar. Some of these may be the same for all cultivars of a
  ! crop, but this structure allows flexibility in terms of which parameters are
  ! crop-specific and which are cultivar-specific.
  !
  ! TODO(wjs, 2019-11-11) We may want to have a base type that includes parameters that
  ! apply to all crops, and then children types for specific crops that hold parameters
  ! specific to that crop.
  type, public :: agsys_cultivar_params_type
     private

     ! Public data members
     integer, allocatable,      public :: max_days_from_sowing_to_end_of_phase(:)
     real(r8),                  public :: p_sowing_depth
     real(r8),                  public :: p_shoot_lag
     real(r8),                  public :: p_shoot_rate
     real(r8), allocatable,     public :: phase_target_tt(:)  !!target thermal time to finish a phase
     type(response_curve_type), public :: target_tt_from_photoperiod_end_of_juvenile
     type(response_curve_type), public :: rc_tair_tt
     type(response_curve_type), public :: rc_tair_vd
     type(response_curve_type), public :: rc_sw_avail_phenol
     type(response_curve_type), public :: rc_sw_emerg_rate
     type(response_curve_type), public :: rc_photoperiod_target_tt
     type(response_curve_type), public :: rc_cumvd_target_tt
     real(r8),                  public :: p_photop_sens
     real(r8),                  public :: p_vern_sens
     real(r8),                  public :: p_pesw_germ
  end type agsys_cultivar_params_type

  ! Each crop has its own vector of cultivar-specific parameters. There is one instance
  ! of this derived type for each crop.
  type, public :: agsys_crop_cultivar_params_type
     private

     ! Public data members
     type(agsys_cultivar_params_type), allocatable, public :: cultivar_params(:)
  end type agsys_crop_cultivar_params_type

end module AgSysParams
