module AgSysCropTypeMaize
  use AgSysKinds,                       only : r8
  use AgSysConstants,                   only : crop_type_maize
  use AgSysPhases,                      only : max_str_len_for_phase_def, &
                                               phase_type_generic, &
                                               phase_type_germinating, phase_type_emerging, phase_type_photo_sensitive
  use AgSysEnvironmentalInputs,         only : agsys_environmental_inputs_type
  use AgSysUtils,                       only : response_curve_type, interpolation
  use AgSysCropTypePhotoSensitive,      only : agsys_crop_type_photosensitive
  implicit none

  type, extends(agsys_crop_type_photosensitive), public :: agsys_crop_type_maize
    private
    !public data members
    real(r8),                  public :: tt_emerg_to_endjuv
    real(r8),                  public :: tt_emerg_to_fi
    real(r8),                  public :: tt_flag_to_flower
    real(r8),                  public :: tt_flower_to_start_grain
    real(r8),                  public :: tt_flower_to_maturity
    real(r8),                  public :: tt_maturity_to_ripe

    real(r8),                  public :: potential_kernel_weight
    real(r8),                  public :: leaf_no_dead_const
    real(r8),                  public :: leaf_no_dead_slope
  contains
    procedure :: init

  end type agsys_crop_type_maize
contains
  subroutine init(this)
    class(agsys_crop_type_maize), intent(inout) :: this

    ! Initialize the parent class
    call this%agsys_crop_type_photosensitive%init()

    this%croptype                   = crop_type_maize

    !!!initialize the parameters
    !!!TODO(pb, 2019-11-14) now we intialize the variables to 0, 
    !!!but later can initialize them into nan
    this%tt_emerg_to_endjuv         = 0._r8
    this%tt_emerg_to_fi             = 0._r8
    this%tt_flag_to_flower          = 0._r8
    this%tt_flower_to_start_grain   = 0._r8
    this%tt_flower_to_maturity      = 0._r8
    this%tt_maturity_to_ripe        = 0._r8
    this%potential_kernel_weight = 0._r8
    this%leaf_no_dead_const      = 0._r8
    this%leaf_no_dead_slope      = 0._r8
    
    !!!initialize the phases
    this%phases%num_phases=11
    this%phases%stage_name=[character(len=max_str_len_for_phase_def) :: &
                                              'sowing', 'germination', 'emergence', 'end_of_juvenile', &
                                              'floral_initiation', 'flag_leaf', 'flowering', &
                                              'start_grain_fill', 'end_grain_fill', 'maturity', 'harvest_ripe', 'end_crop']
    this%phases%phase_name=[character(len=max_str_len_for_phase_def) :: &
                                              'germinating', 'emerging', 'juvenile', &
                                              'photo_sensitive', 'leaf_apperance', 'flag_leaf_to_flowering', &
                                              'flowering_to_grain_filling', 'grain_filling', 'maturing', 'maturity_to_harvest_ripe', 'ready_for_harvesting']
    this%phases%phase_type=[phase_type_germinating, phase_type_emerging, phase_type_generic, &
                            phase_type_photo_sensitive, phase_type_generic, phase_type_generic, &
                            phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic, phase_type_generic]

  end subroutine init
end module AgSysCropTypeMaize
