module AgSysRuntimeConstants

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Runtime constants needed throughout AgSys. These are NOT crop-specific parameters
  ! (which are stored elsewhere); rather, these are more general constants.
  !
  ! Constants should only be added to this module sparingly: this should be reserved for
  ! constants whose use is widespread, inside and/or outside of AgSys (e.g., to define
  ! dimension sizes on restart and/or history files).
  !
  ! !USES:
  use AgSysCropTypeGeneric,         only : agsys_cultivars_of_crop_type
  !
  implicit none
  private
  save

  ! !PUBLIC DATA:
  integer, public, protected :: agsys_max_phases ! maximum number of phases used by any crop

  ! !PUBLIC ROUTINES:
  public :: InitRuntimeConstants ! Initialize runtime constants in this module

  character(len=*), parameter, private :: sourcefile = &
       __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine InitRuntimeConstants(crops)
    !
    ! !DESCRIPTION:
    ! Initialize runtime constants in this module
    !
    ! !ARGUMENTS:
    type(agsys_cultivars_of_crop_type), intent(in) :: crops(:)
    !
    ! !LOCAL VARIABLES:
    integer :: crop_type
    integer :: cultivar
    integer :: num_phases_this_crop

    character(len=*), parameter :: subname = 'InitRuntimeConstants'
    !-----------------------------------------------------------------------

    agsys_max_phases = 0
    do crop_type = 1, ubound(crops,1)
       ! All cultivars of a given crop type have the same number of phases, so just take
       ! information from the first cultivar of each crop type.
       cultivar = 1
       num_phases_this_crop = crops(crop_type)%cultivars(1)%phases%num_phases
       if (num_phases_this_crop > agsys_max_phases) then
          agsys_max_phases = num_phases_this_crop
       end if
    end do

  end subroutine InitRuntimeConstants

end module AgSysRuntimeConstants
