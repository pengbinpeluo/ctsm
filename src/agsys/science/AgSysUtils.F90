module AgSysUtils
  !--------------Description-------------------------------------
  !This module defines some utils functions for AgSys
  !
  use AgSysKinds, only : r8
  
  implicit none
contains
  !-------------------------------------------------
  !more general functions for mathematical operations
  !-------------------------------------------------
  function interpolation(x, xx, yy, CPNum) result(y)
    !!DESCRIPTION:
    !!A general function for piece-wise linear interpolation

    !!ARGUMENTS:
    real(r8), intent(in) :: x         ! the value on the x-axis whose y value is to be interpolated
    real(r8), intent(in) :: xx(:)     ! important transition points in x-axis
    real(r8), intent(in) :: yy(:)     ! corresponding points on y-axis
    integer,  intent(in) :: CPNum     ! number of critical points in the piece-wise linear function

    real(r8) :: y
    integer:: loc

    loc=1
    do while (loc<=CPNum)
      if (x<=xx(1)) then
        y=yy(1)
        exit
      else if (x>xx(CPNum)) then
        y=yy(CPNum)
        exit
      else if ((x>xx(loc)) .and. (x<=xx(loc+1))) then
        y = (yy(loc+1)-yy(loc))/(xx(loc+1)-xx(loc))*(x-xx(loc))+yy(loc)
        exit
      else
        loc=loc+1
      end if
    end do
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
