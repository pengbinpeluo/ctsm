module AgSysReproductive
  use AgSysKinds,               only : r8
  use AgSysCropTypeGeneric,     only : agsys_crop_type_generic
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysUtils,  only : response_curve_type, interpolation, bound, divide
  use AgSysExcepUtils,          only : iulog, endrun
  implicit none

  subroutine dlt_grain_n_conc (crop, ave_temp, swdef_expansion, nfact_grain_conc, grain_n_conc)
    class(agsys_crop_type_generic), intent(in) :: crop 
    real(r8), intent(in) :: ave_temp
    real(r8), intent(in) :: swdef_expansion
    real(r8), intent(in) :: nfact_grain_conc
    real(r8), intent(out) :: grain_n_conc

    real(r8) :: grain_n_temp_fac
    real(r8) :: grain_n_sw_fac
    real(r8) :: grain_n_conc_pot

    grain_n_temp_fac = crop%temp_fac_min + crop%temp_fac_slope * ave_temp
    grain_n_sw_fac = crop%sw_fac_max - crop%sw_fac_slope * swdef_expansion
    grain_n_conc_pot = crop%g_n_conc_min + (crop%g_n_conc_crit - crop%g_n_conc_min) * nfact_grain_conc  !!here g or grain??
    grain_n_conc = grain_n_conc_pot * max (grain_n_temp_fac, grain_n_sw_fac)
  end subroutine dlt_grain_n_conc

  subroutine grain_n_demand(crop, current_stage, dm_green, dm_growth, dm_retranslocation, n_green, 
                            ave_temp, swdef_expansion, nfact_grain_conc, gn_grain_demand)
    class(agsys_crop_type_generic), intent(in) :: crop 
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: dm_green(:)
    real(r8), intent(in) :: dm_growth(:)
    real(r8), intent(in) :: dm_retranslocation(:)
    real(r8), intent(in) :: n_green(:)
    real(r8), intent(in) :: ave_temp
    real(r8), intent(in) :: swdef_expansion
    real(r8), intent(in) :: nfact_grain_conc
    real(r8), intent(out) :: gn_grain_demand

    real(r8) :: dm_grain !!grain mass (g/m2)
    real(r8) :: dm_meal  !!meal mass (g/m2)
    real(r8) :: n_crit_grain, n_max_grain, n_min_grain
    real(r8) :: gn_conc_crit, gn_conc_max, gn_conc_min
    real(r8) :: gn_potential, gn_capacity
    integer :: current_stage_index
    integer :: part_id

    dm_grain = 0._r8
    dm_meal = 0._r8
 
    current_stage_index = floor(current_stage)
    if (crop%phases(current_stage_index)=="grain_fill") then
      do i = 1, crop%grain_part_num
        part_id = crop%grain_part_id(i)
        dm_grain = dm_grain + dm_green(part_id) + dm_growth(part_id) + dm_retranslocation(part_id)
      end do
      n_crit_grain = crop%n_conc_crit_grain * dm_grain
      n_max_grain = crop%n_conc_max_grain * dm_grain
      n_min_grain = crop%n_conc_min_grain * dm_grain

      dm_meal = dm_green(meal_part_id) + dm_growth(meal_part_id) + dm_retranslocation(meal_part_id)
      gn_conc_crit = divide (n_crit_grain, dm_meal, 0.0)
      gn_conc_max = divide (n_max_grain, dm_meal, 0.0)
      gn_conc_min = divide (n_min_grain, dm_meal, 0.0)
 
      call dlt_grain_n_conc(crop, ave_temp, swdef_expansion, nfact_grain_conc, grain_n_conc)
      gn_grain_demand = (dm_growth(meal_part_id) + dm_retranslocation(meal_par_id)) * grain_n_conc

      gn_potential = dm_meal * gn_conc_max
      gn_capacity = gn_potential - n_green(meal_part_id)
      gn_grain_demand = bound (gn_grain_demand, gn_capacity)
    end if
  end subroutine grain_n_demand

  !!!!As for now, there are multiple ways to simulate grain dry matter and nitrogen demands
  !!!!in the generic Plant model of APSIM, which means that the generic Plant model is not
  !!!!that generic. Later efforts can be devoted to unify those pathways.
 
  !!!!For some crops like wheat, simulation of grain growth is determined by grain number
  subroutine grain_number(crop, current_stage, days_in_phase, dm_green, grain_no)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: days_in_phase(:)
    real(r8), intent(inout) :: grain_no
    integer :: current_stage_index
    
    current_stage_index = floor(current_stage)

    if  ((crop%phases(current_stage_index) == "emerging" ) .and. (days_in_phase(current_stage_index)==1)) then
      !seedling has just emerged.
      grain_no = 0._r8
    else if ((crop%phases(current_stage_index) == "flowering" ) .and. (days_in_phase(current_stage_index)==1)) then
      !we are at first day of grainfill.
      if(crop%grain_no_determinant == "stem")
         grain_no = crop%grains_per_gram_stem * dm_green(find_part_id("stem"))
      else if (crop%grain_no_determinant=="ear")
         grain_no = crop%grains_per_gram_stem * dm_green(find_part_id("ear"))
      else
         write(iulog, *) "Unknown Grain Number Determinant Specified"
         call endrun(msg="Unknown Grain Number Determinant Specified")
      end if
    end if
  end subroutine grain_number
  
  subroutine grain_dm_demand_gn(crop, current_stage, g_grain_no, dm_grain, tair, g_dlt_dm_grain_demand)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: g_grain_no
    real(r8), intent(in) :: dm_grain
    real(r8), intent(in) :: tair
    real(r8), intent(out) :: g_dlt_dm_grain_demand

    integer :: current_stage
    real(r8) :: nfact_grain_conc
    real(r8) :: nfact_grain_fill
    real(r8) :: max_dm_grain
    real(r8) :: max_dlt_dm_grain

    current_stage_index = floor(current_stage)

    if  (crop%phases(current_stage_index) == "postflowering" ) then
      if (crop%phases(current_stage_index)== "grainfill") then  !!!TODO: need to implement a function to tell if the crop is in a phase or composite phase
        g_dlt_dm_grain_demand = g_grain_no
                              * crop%potential_grain_filling_rate
                              * interpolation(tair, crop%rc_rel_grainfill)
      else
        !we are in the flowering to grainfill phase
        g_dlt_dm_grain_demand = g_grain_no
                              * crop%potential_grain_growth_rate
                              * interpolation(tair, crop%rel_grainfill)
      end if
      !check that grain growth will not result in daily n conc below minimum conc
      !for daily grain growth
      nfact_grain_conc = get_nfact_grain_conc()
      nfact_grain_fill = min(1._r8, nfact_grain_conc * crop%potential_grain_n_filling_rate / crop%minimum_grain_n_filling_rate)
      g_dlt_dm_grain_demand = g_dlt_dm_grain_demand * nfact_grain_fill

      !check that growth does not exceed maximum grain size
      max_dm_grain = g_grain_no * crop%max_grain_size
      max_dlt_dm_grain = max (max_dm_grain - dm_grain, 0._r8)
      g_dlt_dm_grain_demand = max(0._r8, min (g_dlt_dm_grain_demand, max_dlt_dm_grain))
    else
      g_dlt_dm_grain_demand = 0._r8
    end if
  end subroutine grain_dm_demand_gn


  subroutine grain_n_demand_gn()
  end subroutine grain_n_demand_gn

  !!!!!For some crops like soybean, simulation of grain growth is determined by harvest index
  subroutine grain_dm_demand_hi()
  end subroutine grain_dm_demand_hi() 
end module AgSysReproductive
