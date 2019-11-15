module AgSysCropTypeWheat
  use AgSysKinds,                  only : r8
  use AgSysEnvironmentalInputs,    only : agsys_environmental_inputs_type
  use AgSysUtils,                  only : response_curve_type, interpolation, bound
  use AgSysCropTypePhotoSensitive, only : agsys_crop_type_photosensitive
  
  implicit none 

  !!a wheat type extended from generic crop type
  type, extends(agsys_crop_type_photosensitive), public :: agsys_crop_type_wheat
     private
     !public data members
     type(response_curve_type), public :: rc_cumvd_target_tt
     real(r8),                  public :: p_photop_sens
     real(r8),                  public :: p_vern_sens
     real(r8),                  public :: reqvd              !!required vernalization days
  contains
     procedure :: vernalization     => vernalization_wheat
     procedure :: vern_days         => vern_days_wheat
     procedure :: vern_effect       => vern_effect_wheat
     procedure :: photop_effect     => photop_effect_wheat
     procedure :: get_stress_phenol_emerging_phase  => get_stress_phenol_emerging_phase_wheat
     procedure :: get_stress_phenol_inductive_phase => get_stress_phenol_inductive_phase_wheat
  end type agsys_crop_type_wheat
contains
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
    if (this%reqvd <= 0._r8) then
      reqvd_final = 50._r8
    else
      reqvd_final = this%reqvd
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
