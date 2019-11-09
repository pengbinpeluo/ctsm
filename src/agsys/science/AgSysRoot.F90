module AgSysRoot
  use AgSysKinds,  only : r8
  use AgSysParams, only : response_curve_type
  use AgSysUtils,  only : interpolation, bound

  implicit none
contains
  function get_sw_avail_for_layer (depth) result(sw_avail)
    real(r8), intent(in) :: depth
    real(r8) :: sw_avail
  end function get_sw_avail_for_layer

  function get_sw_avail (root_depth) result(sw_avail)
    real(r8), intent(in) :: root_depth
    real(r8) :: sw_avail
  end function get_sw_avail

  function get_sw_avail_max_for_layer (depth) result(sw_avail_max)
    real(r8), intent(in) :: depth
    real(r8) :: sw_avail_max
  end function get_sw_avail_max_for_layer

  function get_sw_avail_max(root_depth) result(sw_avail_max)
    real(r8), intent(in) :: root_depth
    real(r8) :: sw_avail_max
  end function get_sw_avail_max

  function getSwDefPheno(sw_avail_ratio, rc_sw_avail_phenol) result(SwDefPheno)
    real(r8), intent(in) :: sw_avail_ratio
    type(response_curve_type), intent(in) :: rc_sw_avail_phenol
    real(r8) :: SwDefPheno

    SwDefPheno=interpolation(sw_avail_ratio, rc_sw_avail_phenol%x, rc_sw_avail_phenol%y, rc_sw_avail_phenol%num_pts)
  end function getSwDefPheno

  function getNFactPheno() result(NFactPheno)
    real(r8) :: NFactPheno
    NFactPheno=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function getNFactPheno

  function getPFactPheno() result(PFactPheno)
    real(r8) :: PFactPheno
    PFactPheno=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function getPFactPheno
end module AgSysRoot
