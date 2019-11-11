module AgSysPhenology
  !--------------Description-------------------------------------
  !This module defines some subroutines for crop phenology simulation
  !
  use AgSysKinds,         only : r8
  use AgSysConstants,     only : crop_type_wheat
  use AgSysPhases,        only : agsys_phases_type, composite_phase_type
  use AgSysPhases,        only : phase_type_generic, phase_type_germinating, phase_type_emerging, &
                                 phase_type_photosensitive, phase_type_inductive, &
                                 phase_type_leaf_appearance, phase_type_node_number
  use AgSysPhases,        only : composite_phase_type_vernalization, composite_phase_type_emerge_to_end_of_juvenile
  use AgSysParams,        only : response_curve_type, agsys_cultivar_params_type
  use AgSysRoot,          only : getSwDefPheno, getNFactPheno, getPFactPheno
  use AgSysUtils,         only : interpolation, bound, divide, reals_are_equal
  use AgSysExcepUtils,    only : iulog, endrun 

  implicit none
contains
  !---------------------------------------------------------------
  !Some subroutines
  !
  subroutine DoTimeStep_Phenology(croptype, phases, cultivar_params, &
                                  photoperiod, tair_max, tair_min, tc, &
                                  sw_avail_ratio, pesw_seedlayer, &
                                  days_after_sowing, current_stage, days_in_phase, tt_in_phase, &
                                  days_after_phase, tt_after_phase, cumvd)

    !!---------------------------------------------------------
    !!This is the main process handler - it changes stage number
    !!---------------------------------------------------------
    !!INPUTS: time constant parameters
    integer,                          intent(in) :: croptype
    type(agsys_phases_type),          intent(in) :: phases
    type(agsys_cultivar_params_type), intent(in) :: cultivar_params

    !!INPUTS: time varying forcing variables from hosting land model
    real(r8), intent(in) :: photoperiod     !!photoperiod=day length [h]
    real(r8), intent(in) :: tair_max        !!daily maximum air temperature [K]
    real(r8), intent(in) :: tair_min        !!daily minimum air temperature [K]
    real(r8), intent(in) :: tc              !!daily mean canopy temperature [K]
    real(r8), intent(in) :: sw_avail_ratio  !!soil water available ratio, can be calculated from soil water profile [-]
    real(r8), intent(in) :: pesw_seedlayer        
    !!OUTPUTS: state variables
    integer,  intent(inout) :: days_after_sowing  !!days after sowing, this is an accumulated number since sowing
    real(r8), intent(inout) :: current_stage       !!current stage number
    real(r8), intent(inout) :: days_in_phase(:)   !!days since start of the current phase
    real(r8), intent(inout) :: tt_in_phase(:)     !!thermal time accumulated since the start of the current phase
    real(r8), intent(inout) :: days_after_phase(:)!!days since end of a phase
    real(r8), intent(inout) :: tt_after_phase(:)  !!thermal time accumulated since end of a phase
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
    real(r8) :: TT
    real(r8) :: dltStage
    real(r8) :: new_stage
    real(r8) :: SwDefPheno
    real(r8) :: NFactPheno
    real(r8) :: PFactPheno
    real(r8) :: photop_eff
    real(r8) :: vern_eff

    real(r8) :: stage_index_devel
    real(r8) :: stage_index_new
    real(r8) :: fract_in_old
    real(r8) :: portion_in_new
    real(r8) :: portion_in_old
 
    type(composite_phase_type) :: vernalization_cphase
    type(composite_phase_type) :: eme2ej_cphase  
    
    !!!!do some value initialization 
    dltStage = 0._r8
    dlt_tt_phenol  = 0._r8
    TT=0._r8
    stress_phenol = 1._r8

    !!!!
    current_stage_index = floor(current_stage)

    vernalization_index  = phases%composite_phase_index_from_type(composite_phase_type_vernalization)
    vernalization_cphase = phases%composite_phases(vernalization_index)
    eme2ej_index         = phases%composite_phase_index_from_type(composite_phase_type_emerge_to_end_of_juvenile)
    eme2ej_cphase        = phases%composite_phases(eme2ej_index)
                                       
    if (any(vernalization_cphase%child_phase_id==floor(current_stage))) then
      call veralization(croptype, tair_max, tair_min, tc, cultivar_params%response_curve_vd, cumvd)
    end if
    if ((croptype==crop_type_wheat) .and. (any(eme2ej_cphase%child_phase_id==floor(current_stage)))) then
      photop_eff=wheat_photop_effect(cultivar_params%p_photop_sens, photoperiod)
      !!maximum vernalisation requirement is 50 days
      vern_eff=wheat_vernaliz_effect(cultivar_params%p_vern_sens, cumvd, 50.0_r8)
    else
      photop_eff=1._r8
      vern_eff=1._r8
    end if

    !!!!
    TT=DailyTT(tair_max, tair_min, cultivar_params%response_curve_TT)
    select case (phases%phase_type(current_stage_index))
      case(phase_type_generic)
        SwDefPheno=getSwDefPheno(sw_avail_ratio, cultivar_params%rc_sw_avail_phenol)
        call DoTimeStep_GenericPhase(MaxDaysFromSowingToEndofPhase = cultivar_params%MaxDaysFromSowingToEndofPhase(current_stage_index), &
                                     das = days_after_sowing, & 
                                     TT = TT, &
                                     SWDefPheno = SwDefPheno, &
                                     phase_TargetTT = cultivar_params%phase_TargetTT(current_stage_index), &
                                     phase_TT = tt_in_phase(current_stage_index), &
                                     dlt_tt_phenol = dlt_tt_phenol, &
                                     phase_devel = phase_devel, &
                                     stress_phenol = stress_phenol)
      case(phase_type_germinating)
        call DoTimeStep_GerminatingPhase(MaxDaysFromSowingToEndofPhase = cultivar_params%MaxDaysFromSowingToEndofPhase(current_stage_index), &
                                         das = days_after_sowing, &
                                         TT = TT, &
                                         pesw_germ = cultivar_params%p_pesw_germ, &
                                         pesw_seedlayer = pesw_seedlayer, &
                                         dlt_tt_phenol = dlt_tt_phenol, &
                                         phase_devel = phase_devel)
      case(phase_type_emerging)
        call DoTimeStep_EmergingPhase(croptype = croptype, &
                                      MaxDaysFromSowingToEndofPhase = cultivar_params%MaxDaysFromSowingToEndofPhase(current_stage_index), &
                                      das = days_after_sowing, &
                                      TT = TT, &
                                      sw_avail_ratio = sw_avail_ratio, &
                                      rc_sw_emerg_rate = cultivar_params%rc_sw_emerg_rate, &
                                      vern_eff = vern_eff, &
                                      photop_eff = photop_eff, &
                                      phase_TargetTT = cultivar_params%phase_TargetTT(current_stage_index), &
                                      phase_TT = tt_in_phase(current_stage_index), &
                                      dlt_tt_phenol = dlt_tt_phenol, &
                                      phase_devel = phase_devel, &
                                      stress_phenol = stress_phenol)
      case(phase_type_photosensitive)
        SwDefPheno=getSwDefPheno(sw_avail_ratio, cultivar_params%rc_sw_avail_phenol)
        call DoTimeStep_PhotoPhase(MaxDaysFromSowingToEndofPhase= cultivar_params%MaxDaysFromSowingToEndofPhase(current_stage_index), &
                                   das = days_after_sowing, &
                                   TT = TT, &
                                   SwDefPheno = SwDefPheno, &
                                   photoperiod = photoperiod, &
                                   rc_photoperiod_TargetTT = cultivar_params%rc_photoperiod_TargetTT, &
                                   phase_TT = tt_in_phase(current_stage_index), &
                                   dlt_tt_phenol = dlt_tt_phenol, &
                                   phase_devel = phase_devel, &
                                   stress_phenol = stress_phenol)
      case(phase_type_inductive)
        SwDefPheno=getSwDefPheno(sw_avail_ratio, cultivar_params%rc_sw_avail_phenol)
        NFactPheno=getNFactPheno()
        PFactPheno=getPFactPheno()
        call DoTimeStep_InductivePhase(croptype = croptype, &
                                       MaxDaysFromSowingToEndofPhase = cultivar_params%MaxDaysFromSowingToEndofPhase(current_stage_index), &
                                       das = days_after_sowing, &
                                       TT = TT, &
                                       cumvd = cumvd, &
                                       rc_cumvd_TargetTT = cultivar_params%rc_cumvd_TargetTT, &
                                       SwDefPheno = SwDefPheno, &
                                       NFactPheno = NFactPheno, &
                                       PFactPheno = PFactPheno, &
                                       vern_eff   = vern_eff,   &
                                       photop_eff = photop_eff, &
                                       phase_TargetTT = cultivar_params%phase_TargetTT(current_stage_index), &
                                       phase_TT = tt_in_phase(current_stage_index), &
                                       dlt_tt_phenol = dlt_tt_phenol, &
                                       phase_devel = phase_devel, &
                                       stress_phenol = stress_phenol)
      case(phase_type_leaf_appearance)
        call DOTimeStep_LeafAppearancePhase()
      case(phase_type_node_number)
        call DoTimeStep_NodeNumberPhase() 
    end select

    new_stage = current_stage_index + phase_devel
    dltStage = new_stage - current_stage

    !make sure the index is something we can work with
    if(current_stage_index >= 0) then
      stage_index_devel = current_stage - current_stage_index + dltStage
      if (stage_index_devel >= 1.0_r8) then
        stage_index_new = int(current_stage + min (1.0_r8, dltStage))
        if (stage_index_new >= phases%num_phases) then
          write(iulog, *) "The phenology in ", croptype, " has tried to move to phase number ", stage_index_new,& 
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

    if ((int(current_stage) >= phases%num_phases) .or. (current_stage < 0.0_r8)) then
      write(iulog, *) "Illegal stage number.."
      call endrun(msg="Illegal stage number...")
    end if

    days_after_sowing=days_after_sowing+1
  end subroutine DotimeStep_Phenology

  subroutine Initialize_OnSowing(shoot_lag, shoot_rate, sowing_depth, EmergingPhase_TargetTT)
    real(r8), intent(in) :: shoot_lag    !!shoot lag parameter [degs day]
    real(r8), intent(in) :: shoot_rate   !!shoot rate parameter [degs day per m]
    real(r8), intent(in) :: sowing_depth !!sowing depth of seed [m]
    real(r8), intent(inout) :: EmergingPhase_TargetTT
    
    EmergingPhase_TargetTT=shoot_lag+sowing_depth*shoot_rate
  end subroutine

  subroutine DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in):: TT
    real(r8), intent(in):: phase_TT
    real(r8), intent(in):: phase_TargetTT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
   
    dlt_tt_phenol = TT * stress_phenol

    if (MaxDaysFromSowingToEndofPhase > 0) then
      if (das >= MaxDaysFromSowingToEndofPhase) then
         phase_devel = 1._r8
      else
         phase_devel = 0._r8
      end if
    else
      phase_devel = bound((phase_TT + dlt_tt_phenol)/phase_TargetTT, 0._r8, 1._r8)
    end if
  end subroutine DoTimeStep_Phase

  subroutine DoTimeStep_GerminatingPhase(MaxDaysFromSowingToEndofPhase, das, TT, pesw_germ, pesw_seedlayer, dlt_tt_phenol, phase_devel)
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das         !! days after sowing
    real(r8), intent(in) :: pesw_germ  !! a threshold plant extratable soil water for germination
    real(r8), intent(in) :: pesw_seedlayer  !! plant extratable soil water in the seeding layer
    real(r8), intent(in) :: TT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel

    if (MaxDaysFromSowingToEndofPhase > 0) then
      if (das >= MaxDaysFromSowingToEndofPhase) then
         phase_devel = 1.999_r8
      else
         phase_devel = 0.999_r8
      end if
    else
       !! can't germinate on same day as sowing, because we would miss out on
       !! day of sowing elsewhere.
       if ((das > 0) .and. (pesw_seedlayer>pesw_germ)) then
          phase_devel = 1.999_r8
       else
          phase_devel = 0.999_r8
       end if
    end if
    dlt_tt_phenol = TT
  end subroutine DoTimeStep_GerminatingPhase
 
  subroutine DoTimeStep_EmergingPhase(croptype, MaxDaysFromSowingToEndofPhase, das, TT, &
                                      sw_avail_ratio, rc_sw_emerg_rate, vern_eff, photop_eff, &
                                      phase_TargetTT, phase_TT, &
                                      dlt_tt_phenol, phase_devel, stress_phenol)
    integer, intent(in) :: croptype
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: TT
    real(r8), intent(in) :: phase_TT
    real(r8), intent(in) :: phase_TargetTT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol

    real(r8), intent(in) :: sw_avail_ratio
    type(response_curve_type), intent(in) :: rc_sw_emerg_rate
   
    real(r8), intent(in) :: vern_eff !!!still not sure we should calculate vern_eff and photop_eff within this subroutine or outside?
    real(r8), intent(in) :: photop_eff
   
    if (croptype==crop_type_wheat) then
        !!stress from vernarization, photopheriod, and water deficit
        stress_phenol=min(vern_eff, photop_eff)*rel_emerg_rate(sw_avail_ratio, rc_sw_emerg_rate)
    else
        !!only water stress
        stress_phenol=rel_emerg_rate(sw_avail_ratio, rc_sw_emerg_rate)
    end if
    call DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine DoTimeStep_EmergingPhase

  subroutine DoTimeStep_PhotoPhase(MaxDaysFromSowingToEndofPhase, das, TT, &
                                   SwDefPheno, photoperiod, rc_photoperiod_TargetTT, &
                                   phase_TT, &
                                   dlt_tt_phenol, phase_devel, stress_phenol)
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: TT
    real(r8), intent(in) :: phase_TT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol

    real(r8), intent(in) :: photoperiod
    type(response_curve_type), intent(in) :: rc_photoperiod_TargetTT
    real(r8), intent(in) :: SwDefPheno 
    real(r8) :: phase_TargetTT
 
    phase_TargetTT=TargetTT_from_PhotoPeriod(photoperiod, rc_photoperiod_TargetTT)
    stress_phenol=SwDefPheno
    call DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine DoTimeStep_PhotoPhase

  subroutine DoTimeStep_GenericPhase(MaxDaysFromSowingToEndofPhase, das, TT, &
                                     SwDefPheno, &
                                     phase_TargetTT, phase_TT, &
                                     dlt_tt_phenol, phase_devel, stress_phenol)
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: TT
    real(r8), intent(in) :: phase_TT
    real(r8), intent(in) :: phase_TargetTT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol
    real(r8), intent(in) :: SwDefPheno
    stress_phenol=SwDefPheno
    call DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
  end subroutine DoTimeStep_GenericPhase

  subroutine DoTimeStep_InductivePhase(croptype, MaxDaysFromSowingToEndofPhase, das, TT, &
                                       cumvd, rc_cumvd_TargetTT, &
                                       SwDefPheno, NFactPheno, PFactPheno, vern_eff, photop_eff, &
                                       phase_TargetTT, phase_TT, &
                                       dlt_tt_phenol, phase_devel, stress_phenol)
    integer, intent(in) :: croptype
    integer, intent(in) :: MaxDaysFromSowingToEndofPhase
    integer, intent(in) :: das        !! days after sowing
    real(r8), intent(in) :: TT
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: phase_TT
    real(r8), intent(in) :: phase_TargetTT
    real(r8), intent(inout) :: dlt_tt_phenol
    real(r8), intent(inout) :: phase_devel
    real(r8), intent(inout) :: stress_phenol

    real(r8), intent(in) :: SwDefPheno
    real(r8), intent(in) :: NFactPheno
    real(r8), intent(in) :: PFactPheno
    real(r8), intent(in) :: vern_eff
    real(r8), intent(in) :: photop_eff
    type(response_curve_type), intent(in) :: rc_cumvd_TargetTT

    real(r8) :: phase_TargetTT_new
   
    if (croptype==crop_type_wheat) then
      stress_phenol=min(SwDefPheno, min(NFactPheno, PFactPheno))* min(vern_eff, photop_eff)
      call DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
    else 
      stress_phenol=min(SwDefPheno, min(NFactPheno, PFactPheno))
      phase_TargetTT_new=TargetTT_from_cumvd(cumvd, rc_cumvd_TargetTT)
      call DoTimeStep_Phase(MaxDaysFromSowingToEndofPhase, das, TT, phase_TargetTT_new, phase_TT, dlt_tt_phenol, phase_devel, stress_phenol)
    end if
  end subroutine DotimeStep_InductivePhase

  subroutine DoTimeStep_LeafAppearancePhase()
  end subroutine DoTimeStep_LeafAppearancePhase

  subroutine DoTimeStep_NodeNumberPhase()
  end subroutine DoTimeStep_NodeNumberPhase

  subroutine vernalisation(croptype, tair_max, tair_min, tc, response_curve, cumvd)
    integer, intent(in) :: croptype
    real(r8), intent(in) :: tair_max
    real(r8), intent(in) :: tair_min
    real(r8), intent(in) :: tc
    type(response_curve_type), intent(in) :: response_curve
    real(r8), intent(inout) :: cumvd

    real(r8) :: dlt_cumvd
     
    if (croptype==crop_type_wheat) then
      dlt_cumvd=wheat_vernaliz_days(tair_max, tair_min, tc, cumvd)
    else
      dlt_cumvd=vernaliz_days(tair_max, tair_min, response_curve)
    end if
    cumvd=cumvd + dlt_cumvd
  end subroutine vernalisation 

  !-----------------------------------------------
  !Some functions related with phenology
  !-----------------------------------------------
  function vernaliz_days(tair_max, tair_min, response_curve) result(dlt_cumvd)
    real(r8), intent(in) :: tair_max
    real(r8), intent(in) :: tair_min
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: dlt_cumvd
    integer  :: period, period_num
    real(r8) :: tMean3Hour

    dlt_cumvd=0._r8
    period_num=8  !!3-hourly
    do period = 1, period_num
      tMean3Hour = temp3Hr(tair_max, tair_min, period)
      dlt_cumvd = dlt_cumvd + interpolation(tMean3Hour, response_curve%x, response_curve%y, response_curve%num_pts)
    end do
    dlt_cumvd=dlt_cumvd/period_num
  end function vernaliz_days

  function wheat_vernaliz_days(tair_max, tair_min, tc, cumvd) result(dlt_cumvd)
    real(r8), intent(in) :: tair_max
    real(r8), intent(in) :: tair_min
    real(r8), intent(in) :: tc
    real(r8), intent(in) :: cumvd

    real(r8) :: dlt_cumvd
    real(r8) :: vd, vd1, vd2
    dlt_cumvd=0._r8
    if ((tair_min < 15.0_r8) .and. (tair_max > 0.0_r8)) then
      !!Cold
      vd1 = 1.4_r8 - 0.0778_r8 * tc
      vd2 = 0.5_r8 + 13.44_r8 / ((tair_max-tair_min + 3._r8)**2) * tc
      vd = min (vd1, vd2)
      dlt_cumvd = max (vd, 0.0)
    end if
    if ((tair_max > 30._r8) .and. ((cumvd + dlt_cumvd) < 10._r8)) then
      !!high temperature will reduce vernalization
      dlt_cumvd = - 0.5_r8*(tair_max - 30.0_r8)
      dlt_cumvd = - min(-(dlt_cumvd), cumvd)
    end if
  end function wheat_vernaliz_days

  function wheat_photop_effect(p_photop_sens, photoperiod) result(photop_eff)
    real(r8), intent(in) :: p_photop_sens
    real(r8), intent(in) :: photoperiod
    real(r8) :: photop_eff
    real(r8) :: photop_sens_factor

    photop_sens_factor = p_photop_sens * 0.002_r8
    photop_eff = 1.0_r8 - photop_sens_factor * (20.0_r8 - photoperiod)**2
    photop_eff = bound (photop_eff, 0.0_r8, 1.0_r8)
  end function wheat_photop_effect

  function wheat_vernaliz_effect(p_vern_sens, cumvd, reqvd) result(vern_eff)
    real(r8), intent(in) :: p_vern_sens
    real(r8), intent(in) :: cumvd
    real(r8), intent(in) :: reqvd

    real(r8) :: vern_eff
    real(r8) :: vfac   !! vernalization factor
    real(r8) :: vern_sens_fac
    real(r8) :: reqvd_final

    if (reqvd < 0._r8) then
      reqvd_final = 50._r8
    else
      reqvd_final = reqvd
    end if
    vern_sens_fac = p_vern_sens* 0.0054545_r8 + 0.0003_r8
    vfac = 1.0_r8 - vern_sens_fac * (reqvd_final - cumvd)
    vern_eff = bound (vfac, 0.0_r8, 1.0_r8)
  end function wheat_vernaliz_effect

  function rel_emerg_rate(sw_avail_ratio, response_curve) result(rate)
    !!DESCRIPTION:
    !!for calculating the relative emergence rate from fraction of avaibale soil water
    
    !!ARGUMENTS:
    real(r8),                  intent(in) :: sw_avail_ratio
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: rate

    rate=interpolation(sw_avail_ratio, response_curve%x, response_curve%y, response_curve%num_pts)
  end function rel_emerg_rate
  
  function TargetTT_from_cumvd(cumvd, response_curve) result(TargetTT)
    !!DESCRIPTION:
    !!for calculating the target thermal time for a phase using the cumvd~TargetTT relationship
    !!ARGUMENTS:
    real(r8),                  intent(in) :: cumvd
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: TargetTT
    TargetTT=interpolation(cumvd, response_curve%x, response_curve%y, response_curve%num_pts)
  end function TargetTT_from_cumvd

  function TargetTT_from_PhotoPeriod(photoperiod, response_curve) result(TargetTT)
    !!DESCRIPTION:
    !!for calculating the target thermal time for a phase using the photoperiod~TargetTT relationship
    !!ARGUMENTS:
    real(r8),                  intent(in) :: photoperiod
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: TargetTT
    TargetTT=interpolation(photoperiod, response_curve%x, response_curve%y, response_curve%num_pts)
  end function TargetTT_from_PhotoPeriod

  function DailyTT(tMax, tMin, response_curve) result(TT)
    !!DESCRIPTION:
    !!to calculate the daily thermal time

    !!ARGUMENTS:
    real(r8), intent(in):: tMax  !daily maximum air temperature (K)
    real(r8), intent(in):: tMin  !daily minimum air temperature (K)
    type(response_curve_type), intent(in) :: response_curve

    integer :: period, period_num
    real(r8):: tMean3Hour, TT

    TT = 0
    period_num=8  !!3-hourly
    do period = 1, period_num
      tMean3Hour = temp3Hr(tMax, tMin, period)
      TT = TT + ThermalTime(tMean3Hour-273.15_r8, response_curve)
    end do
    TT=TT/period_num
  end function DailyTT


  function ThermalTime(Temp, response_curve) result(tt)
    !!DESCRIPTION:
    !for calculating thermal time for a particular temperature using the Temp-TT curve

    !!ARGUMENTS:
    real(r8),                  intent(in) :: Temp            ! subdaily temperature (degree C)
    type(response_curve_type), intent(in) :: response_curve
    real(r8) :: tt  !thermal time for output
    tt=interpolation(Temp, response_curve%x, response_curve%y, response_curve%num_pts)
  end function ThermalTime

  function temp3Hr (tMax, tMin, period) result(temp)
    !!DESCRIPTION:
    !!to estimate 3-hourly air temperature based on daily min and max temperature
    !!this is an emprical relationship used in CERES and APSIM model

    !!ARGUMENTS:
    real(r8), intent(in) :: tMax            ! daily max air temperature (K or degree C)
    real(r8), intent(in) :: tMin            ! daily min air temperature (K or degree C)
    integer,  intent(in) :: period      ! count of 3hours (from 1 to 8, unitless)

    real(r8) :: tRangeFract
    real(r8) :: diurnalRange
    real(r8) :: deviation
    real(r8) :: temp

    tRangeFract  = 0.92105_r8+0.1140_r8*period-0.0703_r8*(period**2)+0.0053_r8*(period**3)
    diurnalRange = tMax-tMin
    deviation    = tRangeFract * diurnalRange

    temp = tMin + deviation
  end function temp3Hr

end module AgSysPhenology
