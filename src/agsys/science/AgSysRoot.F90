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
      integer,  public :: total_soil_layer_num !soil layer number
      real(r8), allocatable, public :: soil_layer_depth(:) !for all layers
      real(r8), allocatable, public :: buil_density(:)     !for all layers
      real(r8), allocatable, public :: sat_water_content(:)
      !TODO(pb, 2019-11-14) add more soil properties here
  end type agsys_soil_property_type

  type, public :: agsys_soil_condition_type
      real(r8), public :: sw_avail_ratio    !!!soil water available ratio, can be calculated from soil water profile [-]
      real(r8), public :: pesw_seedlayer    !!!plant extratable soil water at seedling layer
      real(r8), public :: swdef_phenol      !!!soil water stress to phenology
      real(r8), public :: nfact_phenol      !!!nitrogen stress to phenology
      real(r8), public :: pfact_phenol      !!!phosphorus stress to phenology
      real(r8), allocatable, public :: afps(:)
      !TODO(pb, 2019-11-14) add more soil condition variables that are needed by AgSys
  end type agsys_soil_condition_type

contains
  subroutine root_depth_growth(crop, soil_prop, stage, dlt_root_depth)
      class(agsys_crop_type_generic), intent(in) :: crop
      type(agsys_soil_property_type), intent(in) :: soil_prop
      real(r8), intent(in) :: stage
      real(r8), intent(out) :: dlt_root_depth

      real(r8) :: temp_factor
      real(r8) :: ws_factor
      real(r8) :: cum_depth
      real(r8) :: root_depth_in_front_layer
      real(r8) :: weighting_factor
      real(r8) :: fasw1, fasw2, fasw
      real(r8) :: sw_avail_factor
      real(r8) :: afps_factor
      real(r8) :: dlt_root_depth
 
      integer :: front_layer_no
      integer :: next_layer_no

      temp_factor = interpolation(crop%rc_rel_root_advance, avg_temp)  !!!TODO: should we use air tempeature or soil temperature here???
      ws_factor = interpolation(crop%rc_ws_root_fac,sw_def_psn)  !!!effect of supply_demand_ratio (sw_def_psn) on root depth increase
      front_layer_no = find_root_front_layer_no(root_depth)      !!!TODO: we need to implement this
      cum_depth = 0._r8
      do i = 1, front_layer_no
          cum_depth = cum_depth + soil_prop%soil_layer_depth(i)
      end do
      root_depth_in_front_layer = min(max(0._r8, soil_prop%soil_layer_depth(front_layer_no) - (cum_depth - root_depth)), 
                                      soil_prop%soil_layer_depth(front_layer_no))
      weighting_factor = min(max(0._r8, root_depth_in_front_layer / soil_prop%soil_layer_depth(front_layer_no)), 1._r8) 
      next_layer_no = min(front_layer_no + 1, soil_prop%total_soil_layer_num)
      fasw1 = min(max(0._r8, get_fasw_for_layer(front_layer_no)), 1._r8)
      fasw2 = min(max(0._r8, get_fasw_for_layer(next_layer_no)), 1._r8)
      fasw = weighting_factor * fasw2 + (1._r8 - weighting_factor) * fasw1
      sw_avail_factor = interpolation(crop%rc_sw_fac_root, fasw)       !!!water availability for effects on root depth growth
      afps_factor = interpolation(crop%rc_afps_fac_root, afps(layer))  !!!air filled pore space factor for root depth growth
      
      dlt_root_depth  = interpolation(crop%rc_root_depth_rate, stage) *    !!!!root_depth_rate are stage dependent (mm/day)
                        temp_factor *                                      !!!!temperature effect 
                        min(ws_factor, min(sw_avail_factor,afps_factor)) * !!!!water effect
                        soil_prop%xf(front_layer_no)                       !!!!Root exploration factor                          
      
  end subroutine root_depth_growth


  subroutine root_length_growth(crop, soil_prop, dlt_dm_root, root_depth, dlt_root_depth, dlt_root_length, rlv_factor)
      class(agsys_crop_type_generic), intent(in) :: crop
      type(agsys_soil_property_type), intent(in) :: soil_prop
      real(r8), intent(in) :: dlt_dm_root           !!!!g/m2
      real(r8), intent(in) :: root_depth
      real(r8), intent(in) :: dlt_root_depth
      real(r8), intent(inout) :: rlv_factor(:) 
      real(r8), intent(inout) :: dlt_root_length(:) !!!!mm

      real(r8) :: depth_today
      real(r8) :: rlv_factor_tot
      real(r8) :: rld, plant_rld
      real(r8) :: branching_factor
      real(r8) :: dlt_length_tot
      integer :: front_layer_no, layer

      depth_today = root_depth + dlt_root_depth
      front_layer_no = find_root_front_layer_no(root_depth)
      do layer = 0, front_layer_no
          dlt_root_length(layer) = 0._r8 !!!initialize
      end do
      
      rlv_factor_tot = 0._r8
      do layer = 1, front_layer_no
          rld = root_length(layer) / soil_prop%soil_layer_depth(layer)   !!!relative length density
          plant_rld = max(0._r8, rld / crop%plant_density)
          branching_factor = interpolation(crop%rc_rel_root_rate, plant_rld)
          rlv_factor(layer) = interpolation(crop%rc_sw_fac_root, get_sw_avail_ratio(layer)) *
                              interpolation(crop%rc_afps_fac_root, afps(layer)) *
                              branching_factor *             !!!!branching factor
                              soil_prop%xf(layer)  *         !!!!growth factor
                              max(0._r8, soil_prop%soil_layer_depth(layer) / root_depthdlayer[layer])        !!!!space weighting factor
          rlv_factor(layer) = max(rlv_factor(layer), 1e-6_r8)
          rlv_factor_tot = rlv_factor_tot + rlv_factor(layer)
      end do
      dlt_length_tot = dlt_dm_root / sm2smm * crop%specific_root_length
      do layer = 0, front_layer_no
          dlt_root_length(layer) = dlt_length_tot * max (rlv_factor(layer) / rlv_factor_tot, 0._r8)
      end do
  subroutine root_length_growth
  

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
      
      do layer = 0, soil_prop%total_soil_layer_num
          soil_cond%afps(layer) = get_afps (env, soil_prop, layer)
      end do
  end subroutine get_soil_condition

  function get_afps (env, soil_prop, layer) result (afps)
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_soil_property_type), intent(in) :: soil_prop
    integer, intent(in) :: layer
    real(r8) :: afps

    afps = max(0._r8, soil_prop%sat_water_content(layer) - env%h2osoi_liq_24hr(layer)) 
  end function

  function get_sw_avail_ratio (env, root) result(sw_avail_ratio)
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_root_type),                 intent(in) :: root
    real(r8) :: sw_avail_ratio

    !TODO(pb, 2019-11-15) do real calculation later using root and soil water profiles
    sw_avail_ratio=1._r8 
  end function get_sw_avail_ratio

  function get_sw_avail_for_layer (env, depth) result(sw_avail)
    type(agsys_environmental_inputs_type), intent(in) :: env
    real(r8), intent(in) :: depth
    real(r8) :: sw_avail
    !TODO(pb, 2019-11-15) do real calculation later using root and soil depth profiles
    sw_avail = env%h2osoi_liq_24hr(2)  !! current use the second layer which is 0.02-0.06 m
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
