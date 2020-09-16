module AgSysLeaf
  use AgSysKinds,               only : r8

contains

  subroutine CanopyExpansion
    leaf_no_pot
    leaf_area_pot
    leaf_area_stressed
  end subroutine CanopyExpansion
  
  subroutine leaf_no_pot()
    !inout variables
    integer, intent(in) :: leaf_no_pot_option
    
    !output variables
    real(r8), intent(out) :: dltLeafNoPot
    real(r8), intent(out) :: dltNodeNoPot
    real(r8), intent(out) :: gLeavesPerNode
    
    Select case (leaf_no_pot_option)
      case(1)
        call cproc_leaf_no_pot1
      case(2) 
        call cproc_leaf_no_pot2
      case(3)
        call cproc_leaf_no_pot3
    end select
  end subroutine leaf_no_pot

  subroutine cproc_leaf_no_pot1
  end subroutine cproc_leaf_no_pot1
end module AgSysLeaf
