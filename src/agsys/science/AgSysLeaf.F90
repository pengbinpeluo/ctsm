module AgSysLeaf
  use AgSysKinds,               only : r8
  use AgSysExcepUtils,          only : iulog, endrun
  
  integer, parameter, public :: lai_expansion_rate_minval = -1
  integer, parameter, public :: lai_expansion_rate_not_used = 0
  integer, parameter, public :: lai_expansion_rate_default = 1
  integer, parameter, public :: lai_expansion_rate_pp = 2
  integer, parameter, public :: lai_expansion_rate_maxval = 3

contains
  subroutine canopy_expansion_actual(leaf_expansion_rate_option)
    !!------------------------------
    !!determine the actual canopy growth (leaf area, leaf number, and node number) 
    !!using the supply-demand approach
    !!------------------------------
    integer, intent(in) :: leaf_expansion_rate_option
    real(r8), intent(in) :: dlt_lai_stressed
    real(r8), intent(in) :: dlt_leaf_no_pot
    real(r8), intent(in) :: dlt_node_no_pot
    real(r8), intent(in) :: g_lai
    real(r8), intent(in) :: dlt_leaf_dm
    type(response_curve_type), intent(in) :: rc_sla_max
    type(response_curve_type), intent(in) :: rc_leaf_no_frac

    real(r8), intent(out) :: dlt_lai
    real(r8), intent(out) :: dlt_leaf_no
    real(r8), intent(out) :: dlt_node_no

    real(r8) :: sla_max
    real(r8) :: dlt_lai_carbon
    real(r8) :: dlt_lai
    real(r8) :: lai_ratio
    real(r8) :: leaf_no_frac
    
    !!actual change of leaf area index
    if (lai_expansion_rate_option == lai_expansion_rate_pp) then
      !!Set actual dlt LAI as "stressed dlt LAI" without SLA constraints
      dlt_lai = dlt_lai_stressed
    else if (lai_expansion_rate_option == lai_expansion_rate_default) then
      sla_max = interpolation(g_lai, rc_sla_max)  !!calculated daily max spec leaf area
      dlt_lai_carbon = dlt_leaf_dm * sla_max * smm2sm !!maximum daily increase in leaf area
      !!index from carbon supply
      dlt_lai = min(dlt_lai_carbon, dlt_lai_stressed)
    else
      write(iulog, *) leaf_expansion_rate_option, " is not valid!"
      call endrun(msg="invalid_argument: leaf_expansion_rate_option")
    end if

    !!actual change of leaf number
    lai_ratio=max(dlt_lai/dlt_lai_stressed, 0.0) !!ratio of actual to potential lai
    leaf_no_frac = interpolation(lai_ratio, rc_leaf_no_frac)  !!ratio of actual to potential leaf appearance
    dlt_leaf_no = dlt_leaf_no_pot * leaf_no_frac
    
    !!actual change of node number
    if (dlt_leaf_no < dlt_node_no_pot) then
      dlt_node_no = dlt_leaf_no
    else
      dlt_node_no = dlt_node_no_pot
    end if
  end subroutine canopy_expansion_actual

  subroutine canopy_expansion_pot (leaf_no_pot_option, stress_factor, dlt_tt, area_stress_factor)
    real(r8), intent(in) :: leaf_no_pot_option
    real(r8), intent(in) :: stress_factor
    real(r8), intent(in) :: dlt_tt
    real(r8), intent(in) :: area_stress_factor

    call leaf_no_pot(leaf_no_pot_option, stress_factor, dlt_tt)
    if (lai_expansion_rate_option == lai_expansion_rate_pp) then
      call leaf_area_pot_pp()
    else if (lai_expansion_rate_option == lai_expansion_rate_default) then
      call leaf_area_pot()
    else
      write(iulog, *) leaf_expansion_rate_option, " is not valid!"
      call endrun(msg="invalid_argument: leaf_expansion_rate_option")
    end if
    call leaf_area_stressed()
  end subroutine canopy_expansion_pot
  
  subroutine leaf_area_pot(crop, dlt_leaf_no_pot, node_no_now, plant_density, dlt_lai_pot)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in)  :: dlt_leaf_no_pot
    real(r8), intent(in)  :: node_no_now
    real(r8), intent(out) :: dlt_lai_pot
    real(r8) :: leaf_size

    leaf_size = interpolate(node_no_now, crop%rc_leaf_size)
    dlt_lai_pot = dlt_leaf_no_pot * leaf_size * smm2sm * plant_density
  end subroutine leaf_area_pot

  subroutine leaf_area_pot_pp(crop, photo_period, dlt_tt, dlt_lai_tt)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: photo_period
    real(r8), intent(in) :: dlt_tt
    real(r8), intent(out):: dlt_lai_pot
    real(r8) :: lai_rate_photo

    lai_rate_photo = interpolate(photo_period, crop%rc_lai_rate_photo) !Leaf area expansion rate (m^2/m^2/oCd, i.e. LAI/oCd)
    dlt_lai_pot = dlt_tt * lai_rate_photo
  end subroutine leaf_area_potential_pp

  subroutine leaf_no_pot()
    !inout variables
    integer, intent(in) :: leaf_no_pot_option
    
    !output variables
    real(r8), intent(out) :: dlt_leaf_no_pot
    real(r8), intent(out) :: dlt_node_no_pot
    real(r8), intent(out) :: leaves_per_node
    
    Select case (leaf_no_pot_option)
      case(1)
        call cproc_leaf_no_pot(crop, current_stage, dlt_tt, node_no_now, dlt_node_no_pot, dlt_leaf_no_pot)
      case(2)
        call cproc_leaf_no_pot_pp(crop, current_stage, photo_period, dlt_tt, node_no_now, dlt_node_no_pot, dlt_leaf_no_pot)
    end select
  end subroutine leaf_no_pot

  subroutine cproc_leaf_no_pot(crop, current_stage, dlt_tt, node_no_now, dlt_node_no_pot, dlt_leaf_no_pot)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in)  :: current_stage
    real(r8), intent(in)  :: dlt_tt
    real(r8), intent(in)  :: node_no_now
    real(r8), intent(out) :: dlt_node_no_pot
    real(r8), intent(out) :: dlt_leaf_no_pot

    integer  :: node_formation_index
    real(r8) :: node_app_rate
    real(r8) :: leaves_per_node
    type(composite_phase_type) :: node_formation_cphase

    node_formation_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_node_formation)
    if (node_formation_phase_index /= 0) then
      node_formation_cphase = crop%phases%composite_phases(node_formation_index)
      if (any(node_formation_cphase%child_phase_id==floor(current_stage))) then
        node_app_rate = interpolation(node_no_now, crop%rc_node_app_rate)
        dlt_node_no_pot = max(dlt_tt/node_app_rate, 0.0)
        
        leaves_per_node = interpolation(node_no_now, crop%rc_leaves_per_node)
        dlt_leaf_no_pot = dlt_node_no_pot * leaves_per_node
      else
        dlt_node_no_pot = 0.0
        dlt_leaf_no_pot = 0.0
      end if
    else
      dlt_node_no_pot = 0.0
      dlt_leaf_no_pot = 0.0
    end if
  end subroutine cproc_leaf_no_pot
  
  subroutine cproc_leaf_no_pot_pp(crop, current_stage, photo_period, dlt_tt, node_no_now, dlt_node_no_pot, dlt_leaf_no_pot)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in)  :: current_stage
    real(r8), intent(in)  :: photo_period
    real(r8), intent(in)  :: dlt_tt
    real(r8), intent(in)  :: node_no_now
    real(r8), intent(out) :: dlt_node_no_pot
    real(r8), intent(out) :: dlt_leaf_no_pot

    integer  :: node_formation_index
    integer  :: pre_photo_phyllochron_index
    integer  :: photo_phyllochron_index
    integer  :: post_phyllochron_index
    real(r8) :: node_app_rate
    real(r8) :: leaves_per_node
    type(composite_phase_type) :: node_formation_cphase
    type(composite_phase_type) :: pre_photo_phyllochron_cphase
    type(composite_phase_type) :: photo_phyllochron_cphase
    type(composite_phase_type) :: post_photo_phyllochron_cphase

    node_formation_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_node_formation)
    if (node_formation_phase_index /= 0) then
      node_formation_cphase = crop%phases%composite_phases(node_formation_index)
      if (any(node_formation_cphase%child_phase_id==floor(current_stage))) then
        pre_photo_phyllochron_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_pre_photo_phyllochron)
        photo_phyllochron_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_photo_phyllochron)
        post_photo_phyllochron_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_post_photo_phyllochron)
        node_app_rate=0.0
        if ((pre_photo_phyllochron_index /=0) .and. (photo_phyllochron_index /=0) .and. (post_photo_phyllochron_index /=0)) then
          if (any(pre_photo_phyllochron_cphase%child_phase_id==floor(current_stage))) then
            node_app_rate = crop%node_app_rate_pre_photo
          else if (any(photo_phyllochron_cphase%child_phase_id==floor(current_stage))) then
            node_app_rate = interpolation(photo_period, crop%rc_node_app_rate)
          else if (any(post_photo_phyllochron_cphase%child_phase_id==floor(current_stage))) then
            node_app_rate = crop%node_app_rate_post_photo
          else
            write(iulog, *) "invalid_argument: PhotoPhyllochron phases do not align with NodeFormationPhase!"
            call endrun(msg="invalid_argument: PhotoPhyllochron phases do not align with NodeFormationPhase!")
          end if
        end if
        dlt_node_no_pot = max(dlt_tt/node_app_rate, 0.0)
        
        leaves_per_node = interpolation(node_no_now, crop%rc_leaves_per_node)
        dlt_leaf_no_pot = dlt_node_no_pot * leaves_per_node
      else
        dlt_node_no_pot = 0.0
        dlt_leaf_no_pot = 0.0
      end if
    else
      dlt_node_no_pot = 0.0
      dlt_leaf_no_pot = 0.0
    end if
  end subroutine cproc_leaf_no_pot_pp

end module AgSysLeaf
