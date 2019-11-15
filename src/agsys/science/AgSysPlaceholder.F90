module AgSysPlaceholder

  ! TODO(wjs, 2019-11-08) Remove this module!
  use AgSysKinds,               only : r8
  use AgSysCropTypeGeneric,     only : agsys_crop_type_generic
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type
  use AgSysRoot,                only : agsys_soil_condition_type

  implicit none
  private

  public :: DoTimeStep_Phenology_Placeholder

contains

  subroutine DoTimeStep_Phenology_Placeholder(crop, env, soil_cond, &
       days_after_sowing, current_stage, days_in_phase, tt_in_phase, &
       days_after_phase, tt_after_phase, phase_target_tt, cumvd)
    ! Inputs, time-constant
    type(agsys_crop_type_generic), intent(in) :: crop

    ! Inputs, time-varying
    type(agsys_environmental_inputs_type), intent(in) :: env
    type(agsys_soil_condition_type),       intent(in) :: soil_cond

    ! Outputs
    integer,  intent(inout) :: days_after_sowing
    real(r8), intent(inout) :: current_stage
    real(r8), intent(inout) :: days_in_phase(:)    ! for each phase
    real(r8), intent(inout) :: tt_in_phase(:)      ! for each phase
    real(r8), intent(inout) :: days_after_phase(:) ! can be calculated from days_in_phase, so not needed on restart file, but maybe kept as a separate state variable for efficiency (so we don't need to always take the sum of earlier elements of days_in_phase)
    real(r8), intent(inout) :: tt_after_phase(:)   ! similar to days_after_phase
    real(r8), intent(inout) :: phase_target_tt(:)  ! for each phase
    real(r8), intent(inout) :: cumvd ! cumulative vernalization days (ignored for crops without vernalization)
  end subroutine DoTimeStep_Phenology_Placeholder

end module AgSysPlaceholder
