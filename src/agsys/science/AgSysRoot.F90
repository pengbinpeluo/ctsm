module AgSysRoot
  use AgSysKinds,  only : r8
  use AgSysUtils,  only : response_curve_type, interpolation, bound

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

  function get_swdef_phenol(sw_avail_ratio, rc_sw_avail_phenol) result(swdef_phenol)
    real(r8), intent(in) :: sw_avail_ratio
    type(response_curve_type), intent(in) :: rc_sw_avail_phenol
    real(r8) :: swdef_phenol

    swdef_phenol=interpolation(sw_avail_ratio, rc_sw_avail_phenol%x, rc_sw_avail_phenol%y, rc_sw_avail_phenol%num_pts)
  end function get_swdef_phenol

  function get_nFact_phenol() result(nfact_phenol)
    real(r8) :: nfact_phenol
    nfact_phenol=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function get_nfact_phenol

  function get_pfact_phenol() result(pfact_phenol)
    real(r8) :: pfact_phenol
    pfact_phenol=1._r8  !TODO(pb, 2019-11-07) a dummy value here and will come back later
  end function get_pfact_phenol
end module AgSysRoot
