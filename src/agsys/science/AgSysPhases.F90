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
  integer, parameter, public :: phase_type_photo_sensitive = 3
  integer, parameter, public :: phase_type_inductive = 4
  integer, parameter, public :: phase_type_node_number = 5
  integer, parameter, public :: phase_type_leaf_appearance = 6
  integer, parameter, public :: phase_type_end = 7
  integer, parameter, public :: phase_type_maxval = 7

  integer, parameter, public :: composite_phase_type_vernalization = 1
  integer, parameter, public :: composite_phase_type_emerge_to_end_of_juvenile = 2
  integer, parameter, public :: composite_phase_type_maxval = 2

  integer, parameter, public :: max_str_len_for_phase_def = 64  ! maximum number of characters in names of phases / stages / phase types

  ! !PUBLIC TYPES:

  ! Information for a single composite phase for a given crop. A composite phase is
  ! something like vernalization, which is defined as a period spanning multiple real
  ! phases. The real phases included in a composite phase may be contiguous or
  ! non-contiguous.
  type, public :: composite_phase_type
     private

     ! Public data members
     character(len=max_str_len_for_phase_def), public       :: name                ! name of this composite phase
     integer, public                                        :: num_child_phases    ! number of child phases (arrays are dimensioned to be this large)
     integer, allocatable, public                           :: child_phase_id(:)   ! index of each child phase (index into arrays in agsys_phases_type)

     ! TODO(wjs, 2019-11-07) child_phase_name is redundant with information we can already
     ! get from child_phase_id combined with the phase_name array in agsys_phases_type; we
     ! may want to keep this for the sake of consistency checking, but we may want to
     ! remove it.
     character(len=max_str_len_for_phase_def), allocatable, public :: child_phase_name(:) ! name of each child phase
  end type composite_phase_type

  ! Information on the phases for a given crop
  type, public :: agsys_phases_type
     private

     ! Public data members
     integer, allocatable, public :: num_phases ! number of phases for this crop (arrays are dimensioned to be this large)

     ! Each of these arrays are dimensioned by phase. Stage is defined as a point in time
     ! (e.g., the time of sowing, or the time of germination). Phase is defined as the
     ! span of time between two stages (e.g., germinating is the phase between sowing and
     ! germination). 'stage' variables give the stage that occurs at the start of the
     ! given phase. For example, if phase_name(3) = 'juvenile', then the name of the
     ! stage that triggers the start of the juvenile phase is given by stage_name(3)
     ! (e.g., 'emergence').
     character(len=max_str_len_for_phase_def), allocatable, public :: stage_name(:) ! name of the stage that occurs at the start of the given phase
     character(len=max_str_len_for_phase_def), allocatable, public :: phase_name(:) ! name of the given phase
     integer                                 , allocatable, public :: phase_type(:) ! each value is one of the above phase_type_* constants

     ! Composite phases for this crop
     !
     ! 'composite_phases' has one element for each composite phase defined for this crop.
     ! 'composite_phase_index_from_type' lets you find the appropriate composite phase
     ! structure for a given composite phase type (e.g., vernalization).
     !
     ! For example, to find the appropriate composite phase structure for vernalization,
     ! you would do:
     !
     !    vernalization_index = phases%composite_phase_index_from_type(composite_phase_type_vernalization)
     !    phases%composite_phases(vernalization_index)
     !
     ! If the given crop doesn't have a vernalization composite phase, then
     ! phases%composite_phase_index_from_type(composite_phase_type_vernalization) will be
     ! 0. (This error condition doesn't need to be checked explicitly in the code, because
     ! it will lead to a runtime error when we test in debug mode.)
     integer, public :: composite_phase_index_from_type(composite_phase_type_maxval)
     type(composite_phase_type), allocatable, public :: composite_phases(:)

  end type agsys_phases_type
end module AgSysPhases
