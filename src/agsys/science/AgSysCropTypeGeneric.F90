module AgSysCropTypeGeneric
  use AgSysKinds,               only : r8
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysUtils,               only : response_curve_type, interpolation
  use AgSysPhases,              only : agsys_phases_type
  use AgSysExcepUtils,          only : iulog, endrun
  
  implicit none

  !-----------------------------------------------------------------
  !!a generic crop type holding parameters that shared by all crops
  type, public :: agsys_crop_type_generic
     private
     !public data members
     integer,                   public :: croptype

     !phase definition
     type(agsys_phases_type),    public :: phases

     !parameters related with phenology
     integer, allocatable,      public :: max_days_from_sowing_to_end_of_phase(:)
     real(r8),                  public :: p_sowing_depth
     real(r8),                  public :: p_shoot_lag
     real(r8),                  public :: p_shoot_rate
     real(r8),                  public :: p_pesw_germ
     type(response_curve_type), public :: rc_tair_tt
     type(response_curve_type), public :: rc_sw_avail_phenol
     type(response_curve_type), public :: rc_sw_emerg_rate
  contains
     procedure :: init

     procedure :: vernalization
     procedure :: vern_days
     procedure :: vern_effect
     procedure :: photop_effect
     procedure :: get_stress_phenol
     procedure :: get_stress_phenol_emerging_phase
     procedure :: get_stress_phenol_inductive_phase
     procedure :: get_target_tt_photosensitive_phase
     procedure :: get_target_tt_inductive_phase
     procedure :: update_target_tt_for_phases
     procedure :: get_final_leaf_no
  end type agsys_crop_type_generic

  !!a container type to store all the cultivars of a given crop type
  type, public :: agsys_cultivars_of_crop_type
     private
     class(agsys_crop_type_generic), allocatable, public :: cultivars(:)
  end type agsys_cultivars_of_crop_type

contains
  subroutine init(this)
    class(agsys_crop_type_generic), intent(inout) :: this
    this%p_sowing_depth = 0._r8
    this%p_shoot_lag    = 0._r8
    this%p_shoot_rate   = 0._r8
    this%p_pesw_germ    = 0._r8
    this%rc_tair_tt%num_pts = 0
    this%rc_sw_avail_phenol%num_pts = 0
    this%rc_sw_emerg_rate%num_pts = 0

    ! This will be set later, by the specific child class
    this%phases%num_phases = 0
  end subroutine init

  subroutine vernalization(this, env, cumvd)
    class(agsys_crop_type_generic),        intent(in)    :: this
    type(agsys_environmental_inputs_type), intent(in)    :: env
    real(r8),                              intent(inout) :: cumvd
    !do nothing here
  end subroutine vernalization

  function vern_days(this, env, cumvd) result(dlt_cumvd)
    class(agsys_crop_type_generic),        intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8) :: dlt_cumvd
    !do nothng here
  end function vern_days

  function vern_effect(this, cumvd) result(vern_eff)
    class(agsys_crop_type_generic), intent(in) :: this
    real(r8), intent(in) :: cumvd
    real(r8) :: vern_eff
    !do nothing here
  end function vern_effect

  function photop_effect(this, env) result(photop_eff)
    class(agsys_crop_type_generic),        intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8) :: photop_eff
    !do nothing here
  end function photop_effect

  function get_stress_phenol(this, swdef_phenol) result(stress_phenol)
    class(agsys_crop_type_generic), intent(in) :: this
    real(r8), intent(in) :: swdef_phenol
    real(r8) stress_phenol
    !default stress to crop phenology is water stress
    stress_phenol=swdef_phenol
  end function get_stress_phenol

  function get_stress_phenol_inductive_phase(this, env, cumvd, swdef_phenol, nfact_phenol, pfact_phenol) result(stress_phenol)
    class(agsys_crop_type_generic),        intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: swdef_phenol
    real(r8), intent(in) :: nfact_phenol
    real(r8), intent(in) :: pfact_phenol
    real(r8) :: stress_phenol
    !do nothing here
    character(len=*), parameter :: subname = 'get_stress_phenol_inductive_phase'
    write(iulog, *) "This call to ", subname, " is illegal"
    call endrun(msg="Illegal call!")
  end function get_stress_phenol_inductive_phase

  function get_stress_phenol_emerging_phase(this, env, cumvd, sw_avail_ratio) result(stress_phenol)
    class(agsys_crop_type_generic),        intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: sw_avail_ratio
    real(r8) :: stress_phenol

    stress_phenol=interpolation(sw_avail_ratio, this%rc_sw_emerg_rate)
  end function get_stress_phenol_emerging_phase

  function get_target_tt_photosensitive_phase(this, env) result(target_tt)
    class (agsys_crop_type_generic),         intent(in) :: this
    type  (agsys_environmental_inputs_type), intent(in) :: env
    real(r8) :: target_tt
    !do nothing here
    character(len=*), parameter :: subname = 'get_target_tt_photosensitive_phase'
    write(iulog, *) "This call to ", subname, " is illegal"
    call endrun(msg="Illegal call!")
  end function get_target_tt_photosensitive_phase

  subroutine get_target_tt_inductive_phase(this, cumvd, target_tt)
    class(agsys_crop_type_generic), intent(in) :: this
    real(r8), intent(in)    :: cumvd
    real(r8), intent(inout) :: target_tt
    !do nothing here
    character(len=*), parameter :: subname = 'get_target_tt_inductive_phase'
    write(iulog, *) "This call to ", subname, " is illegal"
    call endrun(msg="Illegal call!")
  end subroutine get_target_tt_inductive_phase

  subroutine update_target_tt_for_phases (this, env, das, current_stage_index, phase_target_tt)
    class(agsys_crop_type_generic),         intent(in) :: this
    type (agsys_environmental_inputs_type), intent(in) :: env
    integer,  intent(in)                    :: das
    integer,  intent(in)                    :: current_stage_index
    real(r8), intent(inout)                 :: phase_target_tt(:)
  end subroutine update_target_tt_for_phases
  
  function get_final_leaf_no(this, phase_target_tt) result(final_leaf_no)
    class(agsys_crop_type_generic), intent(in) :: this
    real(r8), intent(in)                       :: phase_target_tt(:)
    real(r8) :: final_leaf_no
  end function get_final_leaf_no
  
end module AgSysCropTypeGeneric
