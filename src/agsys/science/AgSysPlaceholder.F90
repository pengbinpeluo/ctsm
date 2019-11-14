module AgSysPlaceholder

  ! TODO(wjs, 2019-11-08) Remove this module!

  use AgSysKinds , only : r8
  use AgSysPhases, only : agsys_phases_type
  use AgSysEnvironmentalInputs, only : agsys_environmental_inputs_type

  implicit none
  private

  public :: DoTimeStep_Phenology_Placeholder

contains

  subroutine DoTimeStep_Phenology_Placeholder(crop, &
       agsys_environmental_inputs, &
       days_after_sowing, current_stage, days_in_phase, tt_in_phase, &
       days_after_phase, tt_after_phase, cumvd)
    ! Inputs, time-constant
    type(agsys_crop_type_generic), intent(in) :: crop

    ! Inputs, time-varying
    type(agsys_environmental_inputs_type), intent(in) :: agsys_environmental_inputs

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
