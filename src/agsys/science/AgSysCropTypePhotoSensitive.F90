module AgSysCropTypePhotoSensitive
  use AgSysKinds,                only : r8
  use AgSysEnvironmentalInputs,  only : agsys_environmental_inputs_type
  use AgSysUtils,                only : response_curve_type, interpolation
  use AgSysCropTypeGeneric,      only : agsys_crop_type_generic
  implicit none

  type, extends(agsys_crop_type_generic), public :: agsys_crop_type_photosensitive
    private
    !public data members
    type(response_curve_type), public :: rc_photoperiod_target_tt
  contains
    procedure :: init
    procedure :: get_target_tt_photosensitive_phase
  end type agsys_crop_type_photosensitive
contains
  subroutine init(this)
    class(agsys_crop_type_photosensitive), intent(inout) :: this

    ! Initialize the parent class
    call this%agsys_crop_type_generic%init()

    this%rc_photoperiod_target_tt%num_pts = 0
  end subroutine init
  
  function get_target_tt_photosensitive_phase(this, env) result(target_tt)
    class (agsys_crop_type_photosensitive),  intent(in) :: this
    type  (agsys_environmental_inputs_type), intent(in) :: env
    real(r8) :: target_tt
    target_tt = interpolation(env%photoperiod, this%rc_photoperiod_target_tt) 
  end function get_target_tt_photosensitive_phase
end module AgSysCropTypePhotoSensitive
