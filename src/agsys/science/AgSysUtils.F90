module AgSysUtils
  !--------------Description-------------------------------------
  !This module defines some utils functions for AgSys
  !
  use AgSysKinds, only : r8
  
  implicit none

  type, public :: response_curve_type
     private

     ! Public data members
     integer, public :: num_pts
     real(r8), allocatable, public :: x(:)
     real(r8), allocatable, public :: y(:)
  end type response_curve_type

contains
  !-------------------------------------------------
  !more general functions for mathematical operations
  !-------------------------------------------------
  function temp_3hourly (tmax, tmin, period) result(temp)
    !!DESCRIPTION:
    !!to estimate 3-hourly air temperature based on daily min and max temperature
    !!this is an emprical relationship used in CERES and APSIM model

    !!ARGUMENTS:
    real(r8), intent(in) :: tmax            ! daily max air temperature (K or degree C)
    real(r8), intent(in) :: tmin            ! daily min air temperature (K or degree C)
    integer,  intent(in) :: period          ! count of 3hours (from 1 to 8, unitless)

    real(r8) :: t_range_fract
    real(r8) :: diurnal_range
    real(r8) :: deviation
    real(r8) :: temp

    t_range_fract  = 0.92105_r8+0.1140_r8*period-0.0703_r8*(period**2)+0.0053_r8*(period**3)
    diurnal_range  = tmax-tmin
    deviation      = t_range_fract * diurnal_range

    temp = tmin + deviation
  end function temp_3hourly

  function interpolation(x, rc) result(y)
    !!DESCRIPTION:
    !!A general function for piece-wise linear interpolation

    !!ARGUMENTS:
    real(r8), intent(in) :: x                    ! the value on the x-axis whose y value is to be interpolated
    type(response_curve_type), intent(in) :: rc  ! response curve y~f(x)

    real(r8) :: y
    integer:: loc

    loc=1
    if (x<=rc%x(1)) then
      y=rc%y(1)
    else if (x>rc%x(rc%num_pts)) then
        y=rc%y(rc%num_pts)
    else
      do loc=1, rc%num_pts-1
        ! We already know that x > xx(loc) based on checks done in an earlier iteration
        ! (or, for loc = 1, based on the check done above)
        if (x<=rc%x(loc+1)) then
          y = (rc%y(loc+1)-rc%y(loc))/(rc%x(loc+1)-rc%x(loc))*(x-rc%x(loc))+rc%y(loc)
          exit
        end if
      end do
    end if
  end function interpolation
  
  function bound(x, min_value, max_value) result(bounded_x)
    real(r8), intent(in) :: x
    real(r8), intent(in) :: min_value
    real(r8), intent(in) :: max_value
    real(r8) :: bounded_x
    bounded_x=min(max_value, max(min_value, x))
  end function bound

  function divide(divident, divisor, default_value) result(ratio)
    real(r8), intent(in) :: divident
    real(r8), intent(in) :: divisor
    real(r8), intent(in) :: default_value
    real(r8) :: ratio
    
    if (reals_are_equal(divisor, 0._r8)) then
        ratio=default_value
    else if (reals_are_equal(divident, 0._r8)) then
        ratio=0._r8
    else
        ratio=divident/divisor
    end if
  end function divide

  function reals_are_equal(a, b) result(ToF)
    real(r8), intent(in) :: a
    real(r8), intent(in) :: b

    logical :: ToF
    real(r8) :: tol=1.0E-5
    if (abs(a-b)>tol) then
      ToF=.false.
    else
      ToF=.true.
    end if
  end function reals_are_equal

  function itoa(i) result(res)
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(tmp)
  end function
end module AgSysUtils
