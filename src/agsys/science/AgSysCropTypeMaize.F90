module AgSysCropTypeMaize
  use AgSysKinds,                       only : r8
  use AgSysConstants,                   only : crop_type_maize
  use AgSysPhases,                      only : max_str_len_for_phase_def, &
                                               phase_type_generic, &
                                               phase_type_germinating, phase_type_emerging, phase_type_photo_sensitive
  use AgSysEnvironmentalInputs,         only : agsys_environmental_inputs_type
  use AgSysUtils,                       only : response_curve_type, interpolation, bound
  use AgSysCropTypePhotoSensitive,      only : agsys_crop_type_photosensitive
  implicit none

  type, extends(agsys_crop_type_photosensitive), public :: agsys_crop_type_maize
    private
    !public data members
    real(r8),                  public :: target_tt_emerg_to_endjuv
    real(r8),                  public :: target_tt_flag_to_flower
    real(r8),                  public :: target_tt_flower_to_start_grain
    real(r8),                  public :: target_tt_flower_to_maturity
    real(r8),                  public :: target_tt_maturity_to_ripe

    real(r8),                  public :: leaf_init_rate
    real(r8),                  public :: leaf_no_seed
    real(r8),                  public :: leaf_no_at_emerg
    real(r8),                  public :: leaf_no_min
    real(r8),                  public :: leaf_no_max
    real(r8),                  public :: leaf_no_critical
    real(r8),                  public :: leaf_appearance_rate_early
    real(r8),                  public :: leaf_appearance_rate_late
    real(r8),                  public :: potential_kernel_weight
    real(r8),                  public :: leaf_no_dead_const
    real(r8),                  public :: leaf_no_dead_slope
  contains
    procedure :: init
    procedure :: update_target_tt_for_phases
    procedure :: get_final_leaf_no
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
    this%target_tt_emerg_to_endjuv         = 0._r8
    this%target_tt_flag_to_flower          = 0._r8
    this%target_tt_flower_to_start_grain   = 0._r8
    this%target_tt_flower_to_maturity      = 0._r8
    this%target_tt_maturity_to_ripe        = 0._r8

    this%leaf_init_rate             = 0._r8
    this%leaf_no_seed               = 0._r8
    this%leaf_no_at_emerg           = 0._r8
    this%leaf_no_min                = 0._r8
    this%leaf_no_max                = 0._r8
    this%leaf_no_critical           = 0._r8
    this%leaf_appearance_rate_early = 0._r8
    this%leaf_appearance_rate_late  = 0._r8
    this%potential_kernel_weight    = 0._r8
    this%leaf_no_dead_const         = 0._r8
    this%leaf_no_dead_slope         = 0._r8
    
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

  subroutine update_target_tt_for_phases (this, env, das, current_stage_index, phase_target_tt)
    class(agsys_crop_type_maize),           intent(in) :: this
    type (agsys_environmental_inputs_type), intent(in) :: env
    integer,  intent(in)                    :: das
    integer,  intent(in)                    :: current_stage_index
    real(r8), intent(inout)                 :: phase_target_tt(:)
   
    real(r8) :: leaf_no_final
    real(r8) :: target_tt_emerg_to_flag 
    if ((das == 0) .and. (current_stage_index ==1)) then !!! on sowing, I put two triggers here to make sure it is on sowing date
      phase_target_tt(1) = 0._r8  !!germination is determined by soil water in seeding layer, not thermal time
      phase_target_tt(2) = this%p_shoot_lag+this%p_sowing_depth*this%p_shoot_rate
      phase_target_tt(3) = this%target_tt_emerg_to_endjuv
      phase_target_tt(4) = this%rc_photoperiod_target_tt%y(1)
      phase_target_tt(6) = this%target_tt_flag_to_flower
      phase_target_tt(7) = this%target_tt_flower_to_start_grain
      phase_target_tt(9) = this%target_tt_flower_to_maturity*0.01  !!!TODO(pb, 2019-11-15) 0.01 can be a parameter
      phase_target_tt(8) = this%target_tt_flower_to_maturity-phase_target_tt(7)-phase_target_tt(9)
      phase_target_tt(10)= this%target_tt_maturity_to_ripe
      phase_target_tt(11)= 0._r8  !!ripe to harvest, this depends on farmer's behavior
    else if ((current_stage_index >=4) .and. (current_stage_index<5)) then  !!photosensitive phase
      phase_target_tt(4) = this%get_target_tt_photosensitive_phase(env)
    
    else if (current_stage_index>=5) then   !!TODO(pb, 2019-11-15) we only need to do this at one step!
      leaf_no_final=this%get_final_leaf_no(phase_target_tt)
      target_tt_emerg_to_flag=(leaf_no_final-this%leaf_no_critical-this%leaf_no_at_emerg)*this%leaf_appearance_rate_early + &
                           this%leaf_no_critical*this%leaf_appearance_rate_late
      phase_target_tt(5)=target_tt_emerg_to_flag-phase_target_tt(3)-phase_target_tt(4)

    end if 
  end subroutine update_target_tt_for_phases

  function get_final_leaf_no(this, phase_target_tt) result(final_leaf_no)
    class(agsys_crop_type_maize), intent(in) :: this
    real(r8), intent(in)                  :: phase_target_tt(:)
    real(r8) :: final_leaf_no
    
    final_leaf_no =(phase_target_tt(3)+phase_target_tt(4))/this%leaf_init_rate+this%leaf_no_seed
    final_leaf_no = bound (final_leaf_no, this%leaf_no_min, this%leaf_no_max)
  end function get_final_leaf_no

end module AgSysCropTypeMaize
