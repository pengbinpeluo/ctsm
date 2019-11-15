module AgSysRoot
  use AgSysKinds,               only : r8
  use AgSysCropTypeGeneric,     only : agsys_crop_type_generic
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysUtils,  only : response_curve_type, interpolation, bound

  implicit none

  type, public :: agsys_root_type
      real(r8), public :: root_depth
      !TODO(pb, 2019-11-14) add more root properties here
  end type agsys_root_type
  
  type, public :: agsys_soil_property_type
      real(r8), allocatable, public :: soil_layer_depth(:) !for all layers
      real(r8), allocatable, public :: buil_density(:)     !for all layers
      !TODO(pb, 2019-11-14) add more soil properties here
  end type agsys_soil_property_type

  type, public :: agsys_soil_condition_type
      real(r8), public :: sw_avail_ratio    !!!soil water available ratio, can be calculated from soil water profile [-]
      real(r8), public :: pesw_seedlayer    !!!plant extratable soil water at seedling layer
      real(r8), public :: swdef_phenol      !!!soil water stress to phenology
      real(r8), public :: nfact_phenol      !!!nitrogen stress to phenology
      real(r8), public :: pfact_phenol      !!!phosphorus stress to phenology
      !TODO(pb, 2019-11-14) add more soil condition variables that are needed by AgSys
  end type agsys_soil_condition_type

contains
  subroutine get_soil_condition(crop, soil_prop, env, root, soil_cond)
      class(agsys_crop_type_generic),        intent(in)    :: crop
      type(agsys_soil_property_type),        intent(in)    :: soil_prop
      type(agsys_environmental_inputs_type), intent(in)    :: env
      type(agsys_root_type),                 intent(in)    :: root
      type(agsys_soil_condition_type),       intent(inout) :: soil_cond

      soil_cond%sw_avail_ratio = get_sw_avail_ratio(env, root)
      soil_cond%pesw_seedlayer = get_sw_avail_for_layer(env, crop%p_sowing_depth)
      soil_cond%swdef_phenol   = get_swdef_phenol(soil_cond%sw_avail_ratio, crop%rc_sw_avail_phenol)
      soil_cond%nfact_phenol   = get_nfact_phenol()
      soil_cond%pfact_phenol   = get_pfact_phenol()
  end subroutine get_soil_condition

  function get_sw_avail_ratio (env, root) result(sw_avail_ratio)
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_root_type),                 intent(in) :: root
    real(r8) :: sw_avail_ratio 
  end function get_sw_avail_ratio

  function get_sw_avail_for_layer (env, depth) result(sw_avail)
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: depth
    real(r8) :: sw_avail
  end function get_sw_avail_for_layer

  function get_sw_avail (env, root) result(sw_avail)
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_root_type),                 intent(in) :: root
    real(r8) :: sw_avail
  end function get_sw_avail

  function get_sw_avail_max_for_layer (env, depth) result(sw_avail_max)
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: depth
    real(r8) :: sw_avail_max
  end function get_sw_avail_max_for_layer

  function get_sw_avail_max(env, root) result(sw_avail_max)
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_root_type),                 intent(in) :: root
    real(r8) :: sw_avail_max
  end function get_sw_avail_max

  function get_swdef_phenol(sw_avail_ratio, rc_sw_avail_phenol) result(swdef_phenol)
    real(r8), intent(in) :: sw_avail_ratio
    type(response_curve_type), intent(in) :: rc_sw_avail_phenol
    real(r8) :: swdef_phenol

    swdef_phenol=interpolation(sw_avail_ratio, rc_sw_avail_phenol)
  end function get_swdef_phenol

  function get_nfact_phenol() result(nfact_phenol)
    real(r8) :: nfact_phenol
    nfact_phenol=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function get_nfact_phenol

  function get_pfact_phenol() result(pfact_phenol)
    real(r8) :: pfact_phenol
    pfact_phenol=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function get_pfact_phenol
end module AgSysRoot
