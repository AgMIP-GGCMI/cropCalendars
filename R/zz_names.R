#' @title Path and file names of GGCMI climate data
#'
#' @description Paste path and file name of climate NCDF input data for GGCMI phase3. A land-only version of ISIMIP3 climate data. For more information, visit https://protocol.isimip.org/#31-climate-related-forcing
#' @export
paste_ggcmi3_clm_fname <- function(
    path1 = "/my_data_path/phase3/climate_land_only/",
    path2         = "climate3b/",
    clm_scenario  = "picontrol",
    clm_forcing   = "GFDL-ESM4",
    ens_member    = "r1i1p1f1",
    bias_adj      = "w5e5",
    clm_var       = "tas",
    extent        = "global",
    time_step     = "daily",
    start_year    = 1601,
    end_year      = 1610,
    file_ext      = ".nc"
    ) {

    # <climate-scenario>/<climate-forcing>/<climate-forcing>_<ensemble-member>_<bias-adjustment>_<climate-scenario>_<climate-variable>_global_daily_<start-year>_<end-year>.nc
    path <- paste0(path1, path2, clm_scenario, "/", clm_forcing, "/")
    fname <- paste(
        tolower(clm_forcing),
        ens_member,
        bias_adj,
        clm_scenario,
        clm_var,
        extent,
        time_step,
        start_year,
        end_year,
        sep = "_"
    )
    return(paste0(path, fname, file_ext))
}
