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
                              * interpolation(tair, crop%rc_rel_grainfill)
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

  subroutine grain_n_demand_gn(crop, current_stage, g_grain_no, tair, growth_dm, retranslocate_dm, g_n_grain_demand)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: g_grain_no
    real(r8), intent(in) :: tair  !daily mean air temperature
    real(r8), intent(in) :: growth_dm(:)
    real(r8), intent(in) :: retranslocate_dm(:)
    real(r8), intent(out) :: g_n_grain_demand

    integer :: current_stage_index
    integer :: meal_part_id
    real(r8) :: g_n_grain_demand
    real(r8) :: g_n_grain_demand1, g_n_grain_demand2
    real(r8) :: grain_growth
    real(r8) :: daily_n_conc

    current_stage_index = floor(current_stage)

    if (crop%phases(current_stage_index)=="reproductive") then
      !we are in grain filling stage
      g_n_grain_demand1 = g_grain_no
                     * crop%potential_grain_n_filling_rate * get_nfact_grain_conc()
                     * interpolation(tair, crop%rc_rel_grain_n_fill)

      g_n_grain_demand2 = min(g_grain_no * crop%potential_grain_n_filling_rate * interpolation(tair, crop%rc_rel_grain_n_fill),
                              get_n_supply())
      g_n_grain_demand = max(g_n_grain_demand1, g_n_grain_demand2)
      !g_n_grain_demand = g_n_grain_demand1
    end if
    if (crop%phases(current_stage_index)=="postflowering") then
      !during grain C filling period so make sure that C filling is still
      !going on otherwise stop putting N in now

      grain_growth = divide((growth_dm(meal_part_id) + retranslocation_dm(meal_part_id)),
                            g_grain_no, 
                            0._r8)
      if (grain_growth < crop%crit_grainfill_rate) then
        ! grain filling has stopped - stop n flow as well
        g_n_grain_demand = 0.0
      end if
      daily_n_conc = divide(g_n_grain_demand,(growth_dm(meal_part_id) + retranslocation_dm(meal_part_id)), 1._r8)
      if (daily_n_conc > crop%grain_max_daily_n_conc) then
        g_n_grain_demand = (growth_dm(meal_part_id) + retranslocation_dm(meal_part_id)) * crop%grain_max_daily_n_conc
      end if
    end if
  end subroutine grain_n_demand_gn

  !!!!!For some crops like soybean, simulation of grain growth is determined by harvest index
  subroutine grain_dm_demand_hi(crop, current_stage, g_dm_stress_max, photoperiod, dm_green)
  !!!!Find grain demand for carbohydrate using harvest index (g/m^2)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: g_dm_stress_max
    real(r8), intent(in) :: photoperiod
    real(r8), intent(in) :: dlt_dm_tot
    real(r8), intent(in) :: dm_green(:)
    
    integer :: current_stage_index
    real(r8) :: hi_max_pot, hi_incr
    real(r8) :: dm_green_yield_parts
    real(r8) :: dm_tops_new, agb
    real(r8) :: harvest_index, harvest_index_new
    real(r8) :: dlt_dm_yield_unadj, dlt_dm_yield, dlt_dm_grain_demand
    real(r8) :: g_dlt_dm_grain_demand, g_dlt_dm_oil_demand, g_dlt_dm_meal_demand
    real(r8) :: dlt_dm_oil, dlt_dm_oil_conversion

    current_stage_index = floor(current_stage)

    if (crop%phases(current_stage_index)=="grainfill") then
      hi_max_pot = interpolation (g_dm_stress_max, crop%rc_hi_max_pot_stress)
      hi_incr = interpolation (photoperiod, crop%rc_hi_incr_pp)

      dm_green_yield_parts = dm_green(meal_part_id) + dm_green(oil_part_id)
      call get_total_agb(crop, dm_green, agb)
      harvest_index = divide (dm_green_yield_parts, agb, 0.0)
      dm_tops_new = agb + dlt_dm_tot   !!!!unadjusted for Grain Energy in dlt_dm
    
      harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot)   !!increase of harvest index
      dm_grain_new = dm_tops_new * harvest_index_new           !!unadjusted for Grain Energy in dlt_dm
      dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts !!unadjusted for Grain Energy in dlt_dm
    
      !!!!adjust for grain energy
      dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new)
      !! finally adjust for Grain energy used from dlt_dm - this is the potential grain wt
      dlt_dm_yield = dlt_dm_yield_unadj * divide (1.0, 1.0 + harvest_index_new * (g_grain_energy - 1.0), 0.0)
      dlt_dm_grain_demand = dlt_dm_yield * g_grain_energy   !!adding grain energy to potential new grain wt to get grain demand

      g_dlt_dm_grain_demand = dlt_dm_grain_demand

      !!!!allocate demand to oil and meal parts (grain=oil+meal)
      dlt_dm_oil = divide (dlt_dm_grain_demand, g_grain_energy, 0._r8) * crop%grain_oil_conc
      dlt_dm_oil_conversion =  divide(dlt_dm_grain_demand, g_grain_energy, 0._r8) * (g_grain_energy - 1._r8)
      g_dlt_dm_oil_demand = max(0._r8, dlt_dm_oil + dlt_dm_oil_conversion)
      g_dlt_dm_meal_demand = dlt_dm_grain_demand - g_dlt_dm_oil_demand
    end if
  end subroutine grain_dm_demand_hi() 

  subroutine get_total_agb(crop, dm_green, agb)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in)  :: dm_green(:)
    real(r8), intent(out) :: agb
    integer i, part_id

    agb = 0._r8
    do i = 1, crop%top_part_num
      part_id = crop%top_part_id(i)
      agb = agb + dm_green (part_id)
    end do
  end subroutine get_total_agb
end module AgSysReproductive
