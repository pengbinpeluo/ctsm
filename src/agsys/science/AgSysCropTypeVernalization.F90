module AgSysCropTypeVernalization
  use AgSysKinds,                       only : r8
  use AgSysEnvironmentalInputs,         only : agsys_environmental_inputs_type
  use AgSysUtils,                       only : response_curve_type, temp_3hourly, interpolation
  use AgSysCropTypePhotoSensitive,      only : agsys_crop_type_photosensitive
  implicit none

  type, extends(agsys_crop_type_photosensitive), public :: agsys_crop_type_vernalization
     private
     !public data members
     type(response_curve_type), public :: rc_tair_vd
     type(response_curve_type), public :: rc_cumvd_target_tt
  contains
     procedure :: vernalization
     procedure :: vern_days
     procedure :: get_stress_phenol_inductive_phase
  end type agsys_crop_type_vernalization
contains
  subroutine vernalization(this, env, cumvd)
    class(agsys_crop_type_vernalization),  intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(inout) :: cumvd
    real(r8) :: dlt_cumvd
    dlt_cumvd=this%vern_days(env, cumvd)
    cumvd=cumvd + dlt_cumvd
  end subroutine vernalization

  function vern_days(this, env, cumvd) result(dlt_cumvd)
    class(agsys_crop_type_vernalization),  intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8) :: dlt_cumvd
    integer  :: period, period_num
    real(r8) :: tmean_3hour

    dlt_cumvd=0._r8
    period_num=8  !!3-hourly
    do period = 1, period_num
      tmean_3hour = temp_3hourly(env%tair_max, env%tair_min, period)
      dlt_cumvd = dlt_cumvd + interpolation(tmean_3hour, this%rc_tair_vd)
    end do
    dlt_cumvd=dlt_cumvd/period_num
  end function vern_days

  function get_stress_phenol_inductive_phase(this, env, cumvd, swdef_phenol, nfact_phenol, pfact_phenol) result(stress_phenol)
    class(agsys_crop_type_vernalization),  intent(in) :: this
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: swdef_phenol
    real(r8), intent(in) :: nfact_phenol
    real(r8), intent(in) :: pfact_phenol
    real(r8) :: stress_phenol
    stress_phenol=min(swdef_phenol, min(nfact_phenol, pfact_phenol))
  end function get_stress_phenol_inductive_phase

  function get_target_tt_inductive_phase(this, cumvd) result(target_tt)
    class(agsys_crop_type_vernalization), intent(in) :: this
    real(r8), intent(in) :: cumvd
    real(r8) :: target_tt
    target_tt=interpolation(cumvd, this%rc_cumvd_target_tt) 
  end function get_target_tt_inductive_phase 
end module AgSysCropTypeVernalization
