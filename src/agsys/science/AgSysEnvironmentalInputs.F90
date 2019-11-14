module AgSysEnvironmentalInputs

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Derived type holding environmental inputs sent from the host model into AgSys routines
  !
  ! The variables here are purely inputs into AgSys (not set by AgSys)
  !
  ! !USES:
  use AgSysKinds, only : r8

  implicit none
  private

  ! !PUBLIC TYPES:

  type, public :: agsys_environmental_inputs_type
     private

     ! Public data members
     real(r8), public :: photoperiod ! same as day length [h]
     real(r8), public :: tair_max  ! daily max air temperature [K]
     real(r8), public :: tair_min  ! daily minimum air temperature [K]
     real(r8), public :: tc_24hr   ! daily mean canopy temperature [K]
     real(r8), allocatable, public :: h2osoi_liq_24hr(:)  ! daily mean soil liquid content for each soil layer [kg m-2]

   contains
     procedure, public :: Init       ! Allocate space for this instance (but don't set any values)
     procedure, public :: SetValues  ! Set values for the current point
  end type agsys_environmental_inputs_type

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, nlevsoi)
    !
    ! !DESCRIPTION:
    ! Allocate space for this instance (but don't set any values)
    !
    ! This should be called once, in initialization. The purpose of separating this from
    ! SetValues is so that we can just do the memory allocation once, rather than doing
    ! this memory allocation repeatedly for every time step and every point.
    !
    ! !ARGUMENTS:
    class(agsys_environmental_inputs_type), intent(inout) :: this
    integer, intent(in) :: nlevsoi ! number of soil layers
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------

    allocate(this%h2osoi_liq_24hr(nlevsoi))

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine SetValues(this, photoperiod, tair_max, tair_min, tc_24hr, h2osoi_liq_24hr)
    !
    ! !DESCRIPTION:
    ! Set values for the current point
    !
    ! !ARGUMENTS:
    class(agsys_environmental_inputs_type), intent(inout) :: this
    real(r8), intent(in) :: photoperiod ! same as day length [h]
    real(r8), intent(in) :: tair_max  ! daily max air temperature [K]
    real(r8), intent(in) :: tair_min  ! daily minimum air temperature [K]
    real(r8), intent(in) :: tc_24hr   ! daily mean canopy temperature [K]
    real(r8), intent(in) :: h2osoi_liq_24hr(:)  ! daily mean soil liquid content for each soil layer [kg m-2]
    !
    ! !LOCAL VARIABLES:

    character(len=*), parameter :: subname = 'SetValues'
    !-----------------------------------------------------------------------

    this%photoperiod = photoperiod
    this%tair_max = tair_max
    this%tair_min = tair_min
    this%tc_24hr = tc_24hr
    this%h2osoi_liq_24hr(:) = h2osoi_liq_24hr(:)

  end subroutine SetValues

end module AgSysEnvironmentalInputs
