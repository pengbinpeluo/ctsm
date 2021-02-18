module AgSysCropTypeWheat
  use AgSysKinds,                  only : r8
  use AgSysConstants,              only : crop_type_wheat
  use AgSysPhases,                 only : max_str_len_for_phase_def, &
                                          phase_type_generic, &
                                          phase_type_germinating, phase_type_emerging, phase_type_inductive, &
                                          composite_phase_type, &
                                          composite_phase_type_vernalization, &
                                          composite_phase_type_emerge_to_end_of_juvenile
  use AgSysEnvironmentalInputs,    only : agsys_environmental_inputs_type
  use AgSysUtils,                  only : response_curve_type, interpolation, bound
  use AgSysCropTypePhotoSensitive, only : agsys_crop_type_photosensitive
  
  implicit none 

  !!a wheat type extended from generic crop type
  type, extends(agsys_crop_type_photosensitive), public :: agsys_crop_type_wheat
     private
     !public data members
     real(r8),                  public :: p_photop_sens
     real(r8),                  public :: p_vern_sens
     real(r8),                  public :: p_reqvd              !!required vernalization days

     real(r8),                  public :: target_tt_end_of_juvenile
     real(r8),                  public :: target_tt_floral_initiation
     real(r8),                  public :: target_tt_flower
     real(r8),                  public :: target_tt_start_grain_fill
     real(r8),                  public :: target_tt_start_grain_fill_to_maturity
  contains
     procedure :: init
     procedure :: update_target_tt_for_phases
     procedure :: vernalization     => vernalization_wheat
     procedure :: vern_days         => vern_days_wheat
     procedure :: vern_effect       => vern_effect_wheat
     procedure :: photop_effect     => photop_effect_wheat
     procedure :: get_stress_phenol_emerging_phase  => get_stress_phenol_emerging_phase_wheat
     procedure :: get_stress_phenol_inductive_phase => get_stress_phenol_inductive_phase_wheat
  end type agsys_crop_type_wheat
contains
  subroutine init(this)
    class(agsys_crop_type_wheat), intent(inout) :: this

    ! Initialize the parent class
    call this%agsys_crop_type_photosensitive%init()

    this%croptype                   = crop_type_wheat

    !!!initialize the parameters
    !!!TODO(pb, 2019-11-21) now we intialize the variables to 0, 
    !!!but later can initialize them into nan
    this%p_photop_sens=0._r8
    this%p_vern_sens=0._r8
    this%p_reqvd=0._r8
   
    this%target_tt_end_of_juvenile=0._r8
    this%target_tt_floral_initiation=0._r8
    this%target_tt_flower=0._r8
    this%target_tt_start_grain_fill=0._r8
    this%target_tt_start_grain_fill_to_maturity=0._r8

    !!!initialize the phases
    this%phases%num_phases=10
    this%phases%stage_name=[character(len=max_str_len_for_phase_def) :: &
                                              'sowing', 'germination', 'emergence', 'end_of_juvenile', &
                                              'floral_initiation', 'flowering', &
                                              'start_grain_fill', 'end_grain_fill', 'maturity', 'harvest_ripe', 'end_crop']
    this%phases%phase_name=[character(len=max_str_len_for_phase_def) :: &
                                              'germinating', 'emerging', 'juvenile', &
                                              'ej_to_fi', 'fi_to_flower', &
                                              'flowering_to_grain_filling', 'grain_filling', 'maturing', 'maturity_to_harvest_ripe', 'ready_for_harvesting']
    this%phases%phase_type=[phase_type_germinating, phase_type_emerging, phase_type_inductive, &
                            phase_type_inductive, phase_type_inductive, &
                            phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic]

    this%phases%composite_phase_index_from_type=[composite_phase_type_vernalization, composite_phase_type_emerge_to_end_of_juvenile]
 
    allocate(composite_phase_type :: this%phases%composite_phases(2))
    this%phases%composite_phases(1)%name='vernalization'
    this%phases%composite_phases(1)%num_child_phases=3
    this%phases%composite_phases(1)%child_phase_id=[2, 3, 4] 
    !!TODO: should we initialize child_phase_name? If we need these names, we can 
    !! get them from the phase_name by using the child_phase_id. The child_phase_name 
    !! is mostly for self-explaining

    this%phases%composite_phases(2)%name='eme2ej'
    this%phases%composite_phases(2)%num_child_phases=2
    this%phases%composite_phases(2)%child_phase_id=[3, 4]
  end subroutine init

  subroutine update_target_tt_for_phases (this, env, das, current_stage_index, phase_target_tt)
    class(agsys_crop_type_wheat),           intent(in) :: this
    type (agsys_environmental_inputs_type), intent(in) :: env
    integer,  intent(in)                    :: das
    integer,  intent(in)                    :: current_stage_index
    real(r8), intent(inout)                 :: phase_target_tt(:)

    real(r8) :: leaf_no_final
    real(r8) :: target_tt_emerg_to_flag
    if ((das == 0) .and. (current_stage_index ==1)) then !!! on sowing, I put two triggers here to make sure it is on sowing date
      phase_target_tt(1) = 0._r8  !!germination is determined by soil water in seeding layer, not thermal time
      phase_target_tt(2) = this%p_shoot_lag+this%p_sowing_depth*this%p_shoot_rate
      phase_target_tt(3) = this%target_tt_end_of_juvenile
      phase_target_tt(4) = this%target_tt_floral_initiation
      phase_target_tt(5) = this%target_tt_flower
      phase_target_tt(6) = this%target_tt_start_grain_fill
      phase_target_tt(8) = (this%target_tt_flower+this%target_tt_start_grain_fill_to_maturity)*0.05 !!!TODO(pb, 2019-11-21) 0.05 can be a parameter
      phase_target_tt(7) = this%target_tt_start_grain_fill_to_maturity-phase_target_tt(8)
      phase_target_tt(9) = 1._r8
      phase_target_tt(10)= 0._r8
    end if
  end subroutine update_target_tt_for_phases

  subroutine vernalization_wheat(this, env, cumvd)
    class(agsys_crop_type_wheat),          intent(in)    :: this
    type(agsys_environmental_inputs_type), intent(in)    :: env
    real(r8),                              intent(inout) :: cumvd
    real(r8) :: dlt_cumvd

    dlt_cumvd=this%vern_days(env, cumvd)
    cumvd=cumvd + dlt_cumvd
  end subroutine vernalization_wheat

  function vern_days_wheat(this, env, cumvd) result(dlt_cumvd)
    class(agsys_crop_type_wheat),          intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8),                              intent(in) :: cumvd

    real(r8) :: dlt_cumvd
    real(r8) :: vd, vd1, vd2
    dlt_cumvd=0._r8
    if ((env%tair_min < 15.0_r8) .and. (env%tair_max > 0.0_r8)) then
      !!Cold
      vd1 = 1.4_r8 - 0.0778_r8 * env%tc_24hr
      vd2 = 0.5_r8 + 13.44_r8 / ((env%tair_max-env%tair_min + 3._r8)**2) * env%tc_24hr
      vd = min (vd1, vd2)
      dlt_cumvd = max (vd, 0.0)
    end if
    if ((env%tair_max > 30._r8) .and. ((cumvd + dlt_cumvd) < 10._r8)) then
      !!high temperature will reduce vernalization
      dlt_cumvd = - 0.5_r8*(env%tair_max - 30.0_r8)
      dlt_cumvd = - min(-(dlt_cumvd), cumvd)
    end if
  end function vern_days_wheat

  function vern_effect_wheat(this, cumvd) result(vern_eff)
    class(agsys_crop_type_wheat), intent(in) :: this
    real(r8), intent(in) :: cumvd

    real(r8) :: vern_eff
    real(r8) :: vfac   !! vernalization factor
    real(r8) :: vern_sens_fac
    real(r8) :: reqvd_final

    !!default vernalisation requirement is 50 days
    if (this%p_reqvd <= 0._r8) then
      reqvd_final = 50._r8
    else
      reqvd_final = this%p_reqvd
    end if
    vern_sens_fac = this%p_vern_sens* 0.0054545_r8 + 0.0003_r8
    vfac = 1.0_r8 - vern_sens_fac * (reqvd_final - cumvd)
    vern_eff = bound (vfac, 0.0_r8, 1.0_r8)
  end function vern_effect_wheat

  function photop_effect_wheat(this, env) result(photop_eff)
    class(agsys_crop_type_wheat),          intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8) :: photop_eff
    real(r8) :: photop_sens_factor

    photop_sens_factor = this%p_photop_sens * 0.002_r8
    photop_eff = 1.0_r8 - photop_sens_factor * (20.0_r8 - env%photoperiod)**2
    photop_eff = bound (photop_eff, 0.0_r8, 1.0_r8)
  end function photop_effect_wheat

  function get_stress_phenol_emerging_phase_wheat(this, env, cumvd, sw_avail_ratio) result(stress_phenol)
    class(agsys_crop_type_wheat),          intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: sw_avail_ratio 
    real(r8) :: stress_phenol

    stress_phenol=min(this%vern_effect(cumvd), this%photop_effect(env))*interpolation(sw_avail_ratio, this%rc_sw_emerg_rate)
  end function get_stress_phenol_emerging_phase_wheat

  function get_stress_phenol_inductive_phase_wheat(this, env, cumvd, swdef_phenol, nfact_phenol, pfact_phenol) result(stress_phenol)
    class(agsys_crop_type_wheat),          intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: swdef_phenol
    real(r8), intent(in) :: nfact_phenol
    real(r8), intent(in) :: pfact_phenol
    real(r8) :: stress_phenol
    stress_phenol=min(swdef_phenol, min(nfact_phenol, pfact_phenol))* min(this%vern_effect(cumvd), this%photop_effect(env))
  end function get_stress_phenol_inductive_phase_wheat

end module AgSysCropTypeWheat
