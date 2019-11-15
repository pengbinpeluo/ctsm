module AgSysPhenology
  !--------------Description-------------------------------------
  !This module defines some subroutines for crop phenology simulation
  !
  use AgSysKinds,               only : r8
  use AgSysConstants,           only : crop_type_wheat
  use AgSysCropTypeGeneric,     only : agsys_crop_type_generic
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysPhases,              only : composite_phase_type
  use AgSysPhases,              only : phase_type_generic, phase_type_germinating, phase_type_emerging, &
                                       phase_type_photo_sensitive, phase_type_inductive, &
                                       phase_type_leaf_appearance, phase_type_node_number
  use AgSysPhases,              only : composite_phase_type_vernalization, composite_phase_type_emerge_to_end_of_juvenile
  use AgSysRoot,                only : agsys_soil_condition_type
  use AgSysUtils,               only : response_curve_type, temp_3hourly, interpolation, bound, divide, reals_are_equal
  use AgSysExcepUtils,          only : iulog, endrun 

  implicit none
  private

  public :: AgSysRunPhenology

contains
  !---------------------------------------------------------------
  !Some subroutines
  !

  subroutine AgSysRunPhenology(crop, env, soil_cond, &
                               crop_alive, days_after_sowing, current_stage, days_in_phase, tt_in_phase, &
                               days_after_phase, tt_after_phase, phase_target_tt, cumvd)

    !!---------------------------------------------------------
    !!This is the main process handler - it changes stage number
    !!---------------------------------------------------------
    !!INPUTS: time constant parameters
    class(agsys_crop_type_generic), intent(in) :: crop
    
    !!INPUTS: time varying forcing variables from hosting land model
    type (agsys_environmental_inputs_type),   intent(in) :: env
    type (agsys_soil_condition_type),         intent(in) :: soil_cond
    
    !!OUTPUTS: state variables
    logical,  intent(inout) :: crop_alive         !!whether the crop is alive (true between planting and sowing)
    integer,  intent(inout) :: days_after_sowing  !!days after sowing, this is an accumulated number since sowing
    real(r8), intent(inout) :: current_stage      !!current stage number
    real(r8), intent(inout) :: days_in_phase(:)   !!days since start of the current phase
    real(r8), intent(inout) :: tt_in_phase(:)     !!thermal time accumulated since the start of the current phase
    real(r8), intent(inout) :: days_after_phase(:)!!days since end of a phase
    real(r8), intent(inout) :: tt_after_phase(:)  !!thermal time accumulated since end of a phase
    real(r8), intent(inout) :: phase_target_tt(:) !!target thermal time to finish a phase
    real(r8), intent(inout) :: cumvd              !!cumulative vernalization days
    
    !------------------------------- 
    !!!!temporal variables 
    integer :: i
    integer :: current_stage_index
    integer :: vernalization_index
    integer :: eme2ej_index

    real(r8) :: dlt_tt_phenol
    real(r8) :: stress_phenol
    real(r8) :: phase_devel
    real(r8) :: tt
    real(r8) :: dltStage
    real(r8) :: new_stage
    real(r8) :: photop_eff  !TODO(pb, 2019-11-14) can be removed
    real(r8) :: vern_eff    !TODO(pb, 2019-11-14) can be removed

    real(r8) :: stage_index_devel
    real(r8) :: stage_index_new
    real(r8) :: fract_in_old
    real(r8) :: portion_in_new
    real(r8) :: portion_in_old
 
    type(composite_phase_type) :: vernalization_cphase
    type(composite_phase_type) :: eme2ej_cphase  
    
    !!!!do some value initialization 
    dltStage       = 0._r8
    dlt_tt_phenol  = 0._r8
    tt             = 0._r8
    stress_phenol  = 1._r8
    photop_eff     = 1._r8
    vern_eff       = 1._r8
  
    !!!!
    current_stage_index = floor(current_stage)

    vernalization_index  = crop%phases%composite_phase_index_from_type(composite_phase_type_vernalization)
    vernalization_cphase = crop%phases%composite_phases(vernalization_index)
    eme2ej_index         = crop%phases%composite_phase_index_from_type(composite_phase_type_emerge_to_end_of_juvenile)
    eme2ej_cphase        = crop%phases%composite_phases(eme2ej_index)
                                       
    if (any(vernalization_cphase%child_phase_id==floor(current_stage))) then
      call crop%vernalization(env, cumvd)
    end if

    !TODO (pb, 2019-11-14)
    !it seems that we don't need this any more and can delete it 
    !if (any(eme2ej_cphase%child_phase_id==floor(current_stage))) then
    !  photop_eff = crop%photop_effect(env%photoperiod)
    !  vern_eff   = crop%vern_effect(cumvd)
    !end if

    !!!!

    tt=get_daily_tt(env%tair_max, env%tair_min, crop%rc_tair_tt)
    select case (crop%phases%phase_type(current_stage_index))
      case(phase_type_generic)
        call AgSysRunGenericPhase(crop = crop, & 
                                  current_stage_index = current_stage_index, &
                                  das = days_after_sowing, & 
                                  tt = tt, &
                                  swdef_phenol = soil_cond%swdef_phenol, &
                                  phase_tt = tt_in_phase(current_stage_index), &
                                  phase_target_tt = phase_target_tt(current_stage_index), &
                                  dlt_tt_phenol = dlt_tt_phenol, &
                                  phase_devel = phase_devel, &
                                  stress_phenol = stress_phenol)
      case(phase_type_germinating)
        call AgSysRunGerminatingPhase(crop = crop, &
                                      current_stage_index = current_stage_index, &
                                      das = days_after_sowing, &
                                      tt = tt, &
                                      pesw_seedlayer = soil_cond%pesw_seedlayer, &
                                      dlt_tt_phenol = dlt_tt_phenol, &
                                      phase_devel = phase_devel)
      case(phase_type_emerging)
        call AgSysRunEmergingPhase(crop = crop, &
                                   env  = env, &
                                   current_stage_index = current_stage_index, &
                                   das = days_after_sowing, &
                                   tt = tt, &
                                   cumvd = cumvd, &
                                   sw_avail_ratio = soil_cond%sw_avail_ratio, &
                                   phase_tt = tt_in_phase(current_stage_index), &
                                   phase_target_tt = phase_target_tt(current_stage_index), &
                                   dlt_tt_phenol = dlt_tt_phenol, &
                                   phase_devel = phase_devel, &
                                   stress_phenol = stress_phenol)
      case(phase_type_photo_sensitive)
        call AgSysRunPhotoPhase(crop = crop, &
                                env  = env, &
                                current_stage_index = current_stage_index, & 
                                das = days_after_sowing, &
                                tt = tt, &
                                swdef_phenol = soil_cond%swdef_phenol, &
                                phase_tt = tt_in_phase(current_stage_index), &
                                phase_target_tt = phase_target_tt(current_stage_index), &
                                dlt_tt_phenol = dlt_tt_phenol, &
                                phase_devel = phase_devel, &
                                stress_phenol = stress_phenol)
      case(phase_type_inductive)
        call AgSysRunInductivePhase(crop = crop, &
                                    env = env, &
                                    current_stage_index = current_stage_index, &
                                    das = days_after_sowing, &
                                    tt = tt, &
                                    cumvd = cumvd, &
                                    swdef_phenol = soil_cond%swdef_phenol, &
                                    nfact_phenol = soil_cond%nfact_phenol, &
                                    pfact_phenol = soil_cond%pfact_phenol, &
                                    phase_tt = tt_in_phase(current_stage_index), &
                                    phase_target_tt = phase_target_tt(current_stage_index), &
                                    dlt_tt_phenol = dlt_tt_phenol, &
                                    phase_devel = phase_devel, &
                                    stress_phenol = stress_phenol)
      case(phase_type_leaf_appearance)
        call AgSysRunLeafAppearancePhase()
      case(phase_type_node_number)
        call AgSysRunNodeNumberPhase() 
    end select

    new_stage = current_stage_index + phase_devel
    dltStage = new_stage - current_stage

    !make sure the index is something we can work with
    if(current_stage_index >= 0) then
      stage_index_devel = current_stage - current_stage_index + dltStage
      if (stage_index_devel >= 1.0_r8) then
        stage_index_new = int(current_stage + min (1.0_r8, dltStage))
        if (stage_index_new >= crop%phases%num_phases) then
          write(iulog, *) "The phenology in ", crop%croptype, " has tried to move to phase number ", stage_index_new,& 
                          " but there aren't that many phases in the model."
          call endrun(msg="stage_index_new is larger than the total stage number of this crop!")
        end if
        if (reals_are_equal(mod(current_stage,1.0_r8),0.0_r8)) then
          fract_in_old = 1.0_r8 - divide(stage_index_devel - 1.0_r8, dltStage, 0.0_r8)
          portion_in_old = fract_in_old * (dlt_tt_phenol + tt_in_phase(current_stage_index))-tt_in_phase(current_stage_index)
        else
          fract_in_old = 1.0_r8 - divide(stage_index_devel - 1.0_r8, dltStage, 0.0_r8)
          portion_in_old = fract_in_old * dlt_tt_phenol
        end if
        portion_in_new = dlt_tt_phenol - portion_in_old
        
        days_in_phase(current_stage_index)=days_in_phase(current_stage_index)+fract_in_old
        tt_in_phase(current_stage_index)=tt_in_phase(current_stage_index)+portion_in_old
        
        days_in_phase(stage_index_new)=days_in_phase(stage_index_new)+(1.0_r8-fract_in_old)
        tt_in_phase(stage_index_new)=tt_in_phase(stage_index_new)+portion_in_new
      else
        days_in_phase(current_stage_index)=days_in_phase(current_stage_index)+1._r8
        tt_in_phase(current_stage_index)=tt_in_phase(current_stage_index)+dlt_tt_phenol
      end if
    end if

    !!Add a new day to days_after_phase of all phases up to but not including the current phase.
    do i = 1, current_stage_index-1
      days_after_phase(i)=days_after_phase(i)+1._r8
      tt_after_phase(i)=tt_after_phase(i)+dlt_tt_phenol
    end do

    if (phase_devel >= 1.0_r8) then
      current_stage = floor(current_stage + 1.0_r8)
    else
      current_stage = new_stage
    end if

    if ((int(current_stage) >= crop%phases%num_phases) .or. (current_stage < 0.0_r8)) then
      write(iulog, *) "Illegal stage number.."
      call endrun(msg="Illegal stage number...")
    end if

    days_after_sowing=days_after_sowing+1
  end subroutine AgSysRunPhenology

  subroutine initialize_on_sowing(shoot_lag, shoot_rate, sowing_depth, target_tt)
    real(r8), intent(in) :: shoot_lag    !!shoot lag parameter  [deg-days]
    real(r8), intent(in) :: shoot_rate   !!shoot rate parameter [deg-days per mm]
    real(r8), intent(in) :: sowing_depth !!sowing depth of seed [mm]
    real(r8), intent(inout) :: target_tt !!target thermal time for emerging phase [deg-days]
    
    target_tt=shoot_lag+sowing_depth*shoot_rate
  end subroutine initialize_on_sowing

  subroutine AgSysRunPhase(crop, current_stage_index, das, tt, phase_target_tt, phase_tt, dlt_tt_phenol, phase_devel, stress_phenol)
    class(agsys_crop_type_generic), intent(in) :: crop
    integer, intent(in) :: current_stage_index
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in):: tt
    real(r8), intent(in):: phase_tt
    real(r8), intent(in):: phase_target_tt
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
   
    dlt_tt_phenol = tt * stress_phenol

    if (crop%max_days_from_sowing_to_end_of_phase(current_stage_index) > 0) then
      if (das >= crop%max_days_from_sowing_to_end_of_phase(current_stage_index)) then
         phase_devel = 1._r8
      else
         phase_devel = 0._r8
      end if
    else
      phase_devel = bound((phase_tt + dlt_tt_phenol)/phase_target_tt, 0._r8, 1._r8)
    end if
  end subroutine AgSysRunPhase

  subroutine AgSysRunGerminatingPhase(crop, current_stage_index, das, tt, pesw_seedlayer, dlt_tt_phenol, phase_devel)
    class(agsys_crop_type_generic), intent(in) :: crop
    integer,  intent(in) :: current_stage_index
    integer,  intent(in) :: das             !! days after sowing
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: pesw_seedlayer  !! plant extratable soil water in the seeding layer
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel

    if (crop%max_days_from_sowing_to_end_of_phase(current_stage_index) > 0) then
      if (das >= crop%max_days_from_sowing_to_end_of_phase(current_stage_index)) then
         phase_devel = 1.999_r8
      else
         phase_devel = 0.999_r8
      end if
    else
       !! can't germinate on same day as sowing, because we would miss out on
       !! day of sowing elsewhere.
       if ((das > 0) .and. (pesw_seedlayer>crop%p_pesw_germ)) then
          phase_devel = 1.999_r8
       else
          phase_devel = 0.999_r8
       end if
    end if
    dlt_tt_phenol = tt
  end subroutine AgSysRunGerminatingPhase
 
  subroutine AgSysRunEmergingPhase(crop, env, current_stage_index, das, tt, cumvd, &
                                   sw_avail_ratio, &
                                   phase_tt, phase_target_tt, &
                                   dlt_tt_phenol, phase_devel, stress_phenol)
    class(agsys_crop_type_generic),           intent(in) :: crop
    type (agsys_environmental_inputs_type),   intent(in) :: env
    integer,  intent(in) :: current_stage_index
    integer,  intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: sw_avail_ratio
    real(r8), intent(in) :: phase_tt
    real(r8), intent(in) :: phase_target_tt
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
   
    stress_phenol=crop%get_stress_phenol_emerging_phase(env, cumvd, sw_avail_ratio)
    call AgSysRunPhase(crop, current_stage_index, das, tt, phase_target_tt, phase_tt, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine AgSysRunEmergingPhase

  subroutine AgSysRunPhotoPhase(crop, env, current_stage_index, das, tt, &
                                swdef_phenol, &
                                phase_tt, phase_target_tt, &
                                dlt_tt_phenol, phase_devel, stress_phenol)
    class(agsys_crop_type_generic),        intent(in) :: crop
    type(agsys_environmental_inputs_type), intent(in) :: env
    integer,  intent(in) :: current_stage_index
    integer,  intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: swdef_phenol 
    real(r8), intent(in) :: phase_tt
    real(r8), intent(inout) :: phase_target_tt 
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
 
    phase_target_tt = crop%get_target_tt_photosensitive_phase(env)
    stress_phenol   = crop%get_stress_phenol(swdef_phenol)
    call AgSysRunPhase(crop, current_stage_index, das, tt, phase_target_tt, phase_tt, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine AgSysRunPhotoPhase

  subroutine AgSysRunGenericPhase(crop, current_stage_index, das, tt, &
                                  swdef_phenol, &
                                  phase_tt, phase_target_tt, &
                                  dlt_tt_phenol, phase_devel, stress_phenol)
    class(agsys_crop_type_generic), intent(in) :: crop
    integer,  intent(in) :: current_stage_index
    integer,  intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: swdef_phenol
    real(r8), intent(in) :: phase_tt
    real(r8), intent(in) :: phase_target_tt
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
    stress_phenol=crop%get_stress_phenol(swdef_phenol)
    call AgSysRunPhase(crop, current_stage_index, das, tt, phase_target_tt, phase_tt, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine AgSysRunGenericPhase

  subroutine AgSysRunInductivePhase(crop, env, current_stage_index, das, tt, cumvd, &
                                    swdef_phenol, nfact_phenol, pfact_phenol, &
                                    phase_tt, phase_target_tt, &
                                    dlt_tt_phenol, phase_devel, stress_phenol)
    class(agsys_crop_type_generic),          intent(in) :: crop
    type(agsys_environmental_inputs_type),   intent(in) :: env
    integer,  intent(in) :: current_stage_index
    integer,  intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: swdef_phenol
    real(r8), intent(in) :: nfact_phenol
    real(r8), intent(in) :: pfact_phenol
    real(r8), intent(in) :: phase_tt
    real(r8), intent(inout) :: phase_target_tt
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
  
    stress_phenol=crop%get_stress_phenol_inductive_phase(env, cumvd, swdef_phenol, nfact_phenol, pfact_phenol)
    phase_target_tt=crop%get_target_tt_inductive_phase(cumvd)
    call AgSysRunPhase(crop, current_stage_index, das, tt, phase_target_tt, phase_tt, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine AgSysRunInductivePhase

  subroutine AgSysRunLeafAppearancePhase()
  end subroutine AgSysRunLeafAppearancePhase

  subroutine AgSysRunNodeNumberPhase()
  end subroutine AgSysRunNodeNumberPhase

  !-----------------------------------------------
  !Some functions related with phenology
  !-----------------------------------------------
  function get_daily_tt(tmax, tmin, response_curve) result(tt)
    !!DESCRIPTION:
    !!to calculate the daily thermal time

    !!ARGUMENTS:
    real(r8), intent(in):: tmax  !daily maximum air temperature (K)
    real(r8), intent(in):: tmin  !daily minimum air temperature (K)
    type(response_curve_type), intent(in) :: response_curve

    integer :: period, period_num
    real(r8):: tmean_3hour, tt

    tt = 0
    period_num=8  !!3-hourly
    do period = 1, period_num
      tmean_3hour = temp_3hourly(tmax, tmin, period)
      tt = tt + thermal_time(tmean_3hour-273.15_r8, response_curve)
    end do
    tt=tt/period_num
  end function get_daily_tt


  function thermal_time(temp, response_curve) result(tt)
    !!DESCRIPTION:
    !for calculating thermal time for a particular temperature using the Temp-tt curve

    !!ARGUMENTS:
    real(r8),                  intent(in) :: temp            ! subdaily temperature (degree C)
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: tt  !thermal time for output
    tt=interpolation(temp, response_curve)
  end function thermal_time

end module AgSysPhenology
