module AgSysPlaceholder

  ! TODO(wjs, 2019-11-08) Remove this module!

  use AgSysKinds , only : r8
  use AgSysParams, only : agsys_cultivar_params_type
  use AgSysPhases, only : agsys_phases_type

  implicit none
  private

  public :: DoTimeStep_Phenology_Placeholder

contains

  subroutine DoTimeStep_Phenology_Placeholder(croptype, phases, cultivar_params, &
       photoperiod, tair_max, tair_min, tc, h2osoi_liq_24hr, &
       days_after_sowing, current_stage, days_in_phase, tt_in_phase, &
       days_after_phase, tt_after_phase, cumvd)
    ! Inputs, time-constant
    integer, intent(in) :: croptype
    type(agsys_phases_type) :: phases
    type(agsys_cultivar_params_type), intent(in) :: cultivar_params

    ! Inputs, time-varying
    real(r8), intent(in) :: photoperiod ! same as day length [h] (exists: grc%dayl)
    real(r8), intent(in) :: tair_max  ! daily max [K] (exists, at least by end of driver loop)
    real(r8), intent(in) :: tair_min  ! daily minimum [K] (exists)
    real(r8), intent(in) :: tc        ! daily mean canopy temperature [K] (exists: t_veg24_patch)
    real(r8), intent(in) :: h2osoi_liq_24hr(:)  ! 1 .. num_soil_layers

    ! Outputs
    integer, intent(inout) :: days_after_sowing
    real(r8), intent(inout) :: current_stage
    real(r8), intent(inout) :: days_in_phase(:)  ! for each phase
    real(r8), intent(inout) :: tt_in_phase(:)    ! for each phase
    real(r8), intent(inout) :: days_after_phase(:) ! can be calculated from days_in_phase, so not needed on restart file, but maybe kept as a separate state variable for efficiency (so we don't need to always take the sum of earlier elements of days_in_phase)
    real(r8), intent(inout) :: tt_after_phase(:) ! similar to days_after_phase
    real(r8), intent(inout) :: cumvd ! cumulative vernalization days (ignored for crops without vernalization)
  end subroutine DoTimeStep_Phenology_Placeholder

end module AgSysPlaceholder
