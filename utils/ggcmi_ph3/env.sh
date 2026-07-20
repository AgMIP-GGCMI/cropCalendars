# ---------------------------------------------------------------------------- #
# Centralized toolchain / module setup for this pipeline.
#
# Every stage (01-03) runs R 4.3.2 (+ netcdf-c) from the PIK 'piam' module set.
# Each launcher sources this file (at the top, and again inside each sbatch
# --wrap -- the batch shell does not inherit the function) and calls load_r_env.
#
# Off the PIK cluster: replace the function body with whatever puts R (and its
# packages) on PATH in your environment (e.g. conda, spack, apt).
# ---------------------------------------------------------------------------- #

# R + package toolchain (stages 01-03): provides R 4.3.2 and netcdf-c.
load_r_env() {
  source /p/system/modulefiles/defaults/piam/module_load_piam_fast
  # HDF5 file locking fails ("Permission denied" in nc_create) on this shared
  # filesystem when writing NetCDF4 from a login node; disable it so stage 02/03
  # work both interactively and under sbatch.
  export HDF5_USE_FILE_LOCKING=FALSE
}
