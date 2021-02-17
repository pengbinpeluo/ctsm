module AgSysArbitrator
  use AgSysKinds,               only : r8
  use AgSysExcepUtils,          only : iulog, endrun
  use AgSysUtils,               only : interpolation, reals_are_equal, bound
contains

  subroutine partition_dm(crop, current_stage, dm_supply, part_dm_demand, part_dm)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: current_stage
    real(r8), intent(in) :: dm_supply    !total supply of dry mass
    real(r8), intent(in) :: part_dm_demand(:) !dry mass demand from different parts
    real(r8), intent(inout) :: part_dm(:)
    real(r8) :: dm_remaining
    real(r8) :: dlt_dm_tot
    real(r8) :: uptake
    real(r8) :: ratio_root_shoot
    real(r8) :: frac_dm_remaining_in_part
 
    dm_remaining = dm_supply
    dlt_dm_tot = 0.0

    do i = 1, crop%partition_part_num
      if (crop%partition_rules(i) == "magic") then  !!this is for root
        ratio_root_shoot=interpolation(current_stage, crop%rc_ratio_root_shoot)
        uptake=ratio_root_shoot/(ratio_root_shoot+1._r8) * dm_supply
        part_dm(i)=part_dm(i)+uptake
        dm_remaining=dm_remaining-uptake
        dlt_dm_tot=dlt_dm_tot+uptake
      else   !!now dm_remaining is for aboveground (non-root or shoot) part
        if (crop%partition_rules(i) == "demand") then
           uptake = min(part_dm_demand(i), dm_remaining)
        else if (crop%partition_rules(i) == "frac") then
           frac_dm_remaining_in_part=interpolation(current_stage, crop%rc_frac_dm_remaining_in_part(i))
           uptake = min(frac_dm_remaining_in_part * dm_remaining,part_dm_demand(i))
        else if (crop%partition_rules(i) == "remainder") then
           uptake = dm_remaining
        else
           write(iulog, *) crop%partition_rules(i), " is not valid!"
           call endrun(msg="Unknown partition rules: crop%partition_rules")
        end if
        part_dm(i)=part_dm(i)+uptake
        dm_remaining=dm_remaining-uptake
        dlt_dm_tot=dlt_dm_tot+uptake
      end if
    end do

    !!check if the total supply of biomass has been fully allocated
    if (abs(dlt_dm_tot-dm_Supply)>=1.e-4_r8) then
      write(iulog, *) "dlt_dm_tot mass balance is off for crop in AgSys!"
      call endrun(msg="dlt_dm_tot mass balance is off for crop in AgSys!")
    end if
  end subroutine partition_dm

  subroutine partition_nitrogen(crop, n_uptake_sum, n_fix_pot, part_n_demand, part_n)
    !===================================================
    !Calculate the nitrogen partitioning in the plant

    !find the proportion of uptake to be distributed to
    !each plant part and distribute it.
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: n_uptake_sum !!total plant N uptake (g/m^2)
    real(r8), intent(in) :: n_fix_pot
    real(r8), intent(in) :: part_n_demand
    real(r8), intent(inout) :: part_n(:)

    real(r8) :: n_demand_sum
    real(r8) :: n_excess
    real(r8) :: dlt_n_tot
    real(r8) :: n_fix_demand_sum
    real(r8) :: n_fix_uptake
    real(r8) :: part_n_demand_differential
    real(r8) :: n_demand_differential_sum
 
    if (reals_are_equal(n_uptake_sum, 0._r8)) then
      n_uptake_sum = 0._r8
    end if
     
    !TODO: need to add code to calculate n_demand_sum from part_n_demand(i) here
    
    n_excess = n_uptake_sum - n_demand_sum
    n_excess = l_bound (n_excess, 0.0)
    dlt_n_tot=0._r8
    do i=1, crop%partition_part_num
      if (n_excess>0.0) then
        part_n(i) = part_n(i) + part_n_demand(i) !!first meet the demand
        dlt_n_tot = dlt_n_tot + part_n_demand(i)
        plant_part_fract = divide (part_n_capacity(i), n_capacity_sum, 0.0) !!then allocate the excess nitrogen
        part_n(i) = part_n(i) + n_excess * plant_part_fract
        dlt_n_tot = dlt_n_tot + n_excess * plant_part_fract
      else
        plant_part_fract = divide (part_n_demand(i), n_demand_sum, 0.0)
        part_n(i) = part_n(i) + n_supply * plant_part_fract
        dlt_n_tot = dlt_n_tot + n_supply * plant_part_fract
      end if
    end do

    !Check Mass Balance
    if (not reals_are_equal(dlt_n_tot - n_uptake_sum, 0._r8)) then
      write(iulog, *) "dlt_n_tot mass balance is off for crop in AgSys!"
      call endrun(msg="dlt_n_tot mass balance is off for crop in AgSys!")
    end if

    !partition N Fixed
    n_fix_demand_sum = max (n_demand_sum - n_uptake_sum, 0._r8) ! total demand for N fixation (g/m^2)
    n_fix_uptake = bound (n_fix_pot, 0.0, n_fix_demand_sum)
    n_demand_differential_sum=n_demand_sum-n_sum
    do i=1, crop%partition_part_num
      part_n_demand_differential=part_n(i)-part_n_demand(i)
      part_n(i)=part_n(i)+n_fix_uptake * divide (part_n_demand_differential, n_demand_differential_sum, 0.0)
    end do
  end subroutine partition_nitrogen

  subroutine retranslocate_dm(crop, part_dm_demand, part_dm_trans_supply, part_dm)
  !+  Purpose
  !   Calculate plant dry matter delta's due to retranslocation
  !   to grain, pod and energy (g/m^2)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: part_dm_demand(:)
    real(r8), intent(in) :: part_dm_trans_supply(:)
    real(r8), intent(inout) :: part_dm(:)

    real(r8) :: dlt_dm_retrans_part                    !carbohydrate removed from part (g/m^2)
    real(r8) :: dm_part_avail                          !carbohydrate avail from part(g/m^2)
    real(r8) :: dm_retranslocate

    real(r8) :: dm_demand_differential_begin
    real(r8) :: dm_demand_differential
    real(r8) :: frac
    integer :: i, ip
    !- Implementation Section ----------------------------------


    ! now translocate carbohydrate between plant components
    ! this is different for each stage

    !plant.All().dlt_dm_green_retrans_hack( 0.0 )  !TODO: need to come back to this

    dm_demand_differential_begin=0._r8
    do i=0, crop%dm_retrans_demand_part_num
      ip=crop%dm_retrans_demand_part_id(i)
      part_dm_demand_differential(i)=part_dm(ip)-part_dm_demand(ip)
      dm_demand_differential_begin=dm_demand_differential_begin+part_dm_demand_differential(i)
    end do

    dm_demand_differential=dm_demand_differential_begin
    dm_retranslocate=0._r8
    ! get available carbohydrate from supply pools
    do i =0,crop%dm_retrans_supply_part_num
      ip=crop%dm_retrans_supply_part_id(i)
      dm_part_avail = part_dm_retrans_supply(ip)
      dlt_dm_retrans_part = min (dm_demand_differential, dm_part_avail)

      !assign and accumulate
      part_dm(ip)=part_dm(ip)-dlt_dm_retrans_part
      dm_retranslocate = dm_retranslocate + dlt_dm_retrans_part
      dm_demand_differential = dm_demand_differential - dlt_dm_retrans_part
    end do

    do i=0, crop%dm_retrans_demand_part_num
      ip=crop%dm_retrans_demand_part_id(i)
      frac=part_dm_demand_differential(i)/dm_demand_differential_begin
      part_dm(ip)=part_dm(ip)+dm_retranslocate*frac
    end do

    write(iulog, *) "Arbitrator.FinalRetranslocation=", dm_retranslocate
  end subroutine retranslocate_dm

  subroutine retranslocte_nitrogen(crop, g_grain_n_demand, part_n)
  !!this subroutine is only for retranslocate the nitrogen of non-root and non-grain parts (stem, leaves, etc)
  !!to the grain part 
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: g_grain_n_demand
    real(r8), intent(inout) :: part_n(:)
    real(r8) :: available_retranslocate_n(:)
    real(r8) :: n_supply_for_retranslocate
    real(r8) :: amount_n_to_retranslocate
    real(r8) :: amount_n_to_retranslocate_total
    integer :: i, ip

    n_supply_for_retranslocate=0._r8
    do i=0, crop%n_retrans_supply_part_num
      ip=crop%n_retrans_supply_part_id(i)
      available_retranslocate_n(ip)=get_available_retranslocate_n(crop, ip)  !!TODO(add this subroutine later, binpeng) 
      n_supply_for_retranslocate=n_supply_for_retranslocate + available_retranslocate_n(ip)
    end do

    amount_n_to_retranslocate_total=0._r8
    do i=0, crop%n_retrans_supply_part_num
      ip=crop%n_retrans_supply_part_id(i)
      amount_n_to_retranslocate=0._r8
      if (g_grain_n_demand >= n_supply_for_retranslocate) then
        !demand greater than or equal to supply
        !retranslocate all available N
        amount_n_to_retranslocate = available_retranslocate_n(ip)
      else
        !supply greater than demand
        !Retranslocate what is needed
        amount_n_to_retranslocate = g_grain_n_demand * divide (available_retranslocate_n(ip), n_supply_for_retranslocation, 0.0)
      end if
      amount_n_to_retranslocate_total=amount_n_to_retranslocate_total + amount_n_to_retranslocate

      !!!reduce from the supply part
      part_n(ip)=part_n(ip)-amount_n_to_retranslocate
    end do

    !!!add to grain part
    ip=crop%n_retrans_demand_part_id
    part_n(ip)=part_n(ip)+amount_n_to_retranslocate_total

  end subroutine retranslocate_nitrogen

  subroutine retranslocate_senescing_nitrogen(crop, green_n, green_dm, senecscing_dm)
  !!Derives seneseced plant nitrogen (g N/m^2)
    class(agsys_crop_type_generic), intent(in) :: crop
    real(r8), intent(in) :: green_n
    real(r8), intent(in) :: green_dm
    real(r8), intent(in) :: senecsing_dm

    do i=0, crop%parttion_part_num
      green_n_conc=divide (green_n(i), green_dm(i), 0.0)
      dlt_n_in_senescing_leaf = senescing_dm * green_n_conc
      sen_n_conc = min (crop%.n_sen_conc(i), green_n_conc)
      navail = dlt_n_in_senescing_leaf - senescing_dm*sen_n_conc
      navail = bound(navail, 0.0, n_demand_tot)
    end do
  end subroutine retranslocate_senesced_nitrogen

end module AgSysArbitrator
