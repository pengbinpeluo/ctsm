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
  use AgSysPhases, only : agsys_phases_type
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
  subroutine InitRuntimeConstants(crop_phases)
    !
    ! !DESCRIPTION:
    ! Initialize runtime constants in this module
    !
    ! !ARGUMENTS:
    type(agsys_phases_type), intent(in) :: crop_phases(:) ! phases for each crop
    !
    ! !LOCAL VARIABLES:
    integer :: crop

    character(len=*), parameter :: subname = 'InitRuntimeConstants'
    !-----------------------------------------------------------------------

    agsys_max_phases = 0
    do crop = 1, ubound(crop_phases,1)
       if (crop_phases(crop)%num_phases > agsys_max_phases) then
          agsys_max_phases = crop_phases(crop)%num_phases
       end if
    end do

  end subroutine InitRuntimeConstants

end module AgSysRuntimeConstants
