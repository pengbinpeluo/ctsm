# We generally list sources in a CMakeLists.txt file in the directory in
# which those sources actually appear. However, we want to avoid needing
# a CMakeLists.txt file in the science directory, to keep out anything
# CTSM-specific from that directory. So we list files in the science
# subdirectory here. Then, for symmetry, we do the same for the other
# agsys subdirectories.
list(APPEND clm_sources
  ctsm_wrappers/AgSysKinds.F90
  science/AgSysConstants.F90
  science/AgSysPhases.F90)

sourcelist_to_parent(clm_sources)

