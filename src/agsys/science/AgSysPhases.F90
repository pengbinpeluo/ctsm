module AgSysPhases

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Derived type describing the translation between a stage index and the phase / stage
  ! names or phase type.
  !
  ! !USES:

  !
  implicit none
  private

  ! !PUBLIC DATA:

  integer, parameter, public :: phase_type_minval = -1
  integer, parameter, public :: phase_type_unused = -1
  integer, parameter, public :: phase_type_generic = 0
  integer, parameter, public :: phase_type_germinating = 1
  integer, parameter, public :: phase_type_emerging = 2
  integer, parameter, public :: phase_type_photosensitive = 3
  integer, parameter, public :: phase_type_inductive = 4
  integer, parameter, public :: phase_type_node_number = 5
  integer, parameter, public :: phase_type_leaf_appearance = 6
  integer, parameter, public :: phase_type_end = 7
  integer, parameter, public :: phase_type_maxval = 7

  integer, parameter, public :: max_phase_name_len = 64  ! maximum number of characters in names of phases / stages

  ! !PUBLIC TYPES:
  type, public :: agsys_phases_type
     private

     ! Public data members
     integer, public :: max_phases ! maximum number of phases used by any crop (arrays are dimensioned to be this large)
     integer, allocatable, public :: num_phases_for_crop(:) ! [crop_type] number of phases used by each crop

     ! Each of these arrays are dimensioned [stage, crop_type]. Stage is defined as a
     ! point in time (e.g., the time of sowing, or the time of germination). Phase is
     ! defined as the span of time between two stages (e.g., germinating is the phase
     ! between sowing and germination). 'phase' variables give the phase whose start point
     ! is the given stage. For example, if crop type 7 has the first two stages 'sowing'
     ! and 'germination', then phase_name(1,7) = 'germinating'. Conversely, if
     ! phase_name(3,7) = 'juvenile', then the name of the stage that triggers the start
     ! of the juvenile phase is given by stage_name(3,7) (e.g., 'emergence').
     character(len=max_phase_name_len), allocatable, public :: stage_name(:,:) ! [stage, crop_type]; name of the given stage
     character(len=max_phase_name_len), allocatable, public :: phase_name(:,:) ! [stage, crop_type]; name of the phase whose start point is the given stage
     integer                          , allocatable, public :: phase_type(:,:) ! [stage, crop_type]; each value is one of the above phase_type_* constants
  end type agsys_phases_type

  type, public :: composite_phase_type
     private
     character(len=max_phase_name_len), allocatable, public :: child_phase(:)
     integer,                           allocatable, public :: child_phase_id(:) 
  end type composite_phase_type

  type, public :: agsys_composite_phase_type
     private
     !public data members
     integer, public :: max_composite_phases !maximum number of composite phases used by any crop (arrays are dimensioned to be this large)
     integer, allocatable, public :: num_composite_phases_for_crop(:) ![crop_type] number of composite phases used by each crop
     
     character(len=max_phase_name_len), allocatable, public :: composite_phase_name(:, :)     ![composite_phase, crop_type]
     type(composite_phase_type), allocatable, public :: composite_phase_children(:, :) ![composite_phase, crop_type]
  end type agsys_composite_phase_type

end module AgSysPhases
