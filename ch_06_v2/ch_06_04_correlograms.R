
# ---- correlogram_response ----
{
  
  newdata = data.frame(
    elevation_sc =   
      seq(
        min(thrush_sf$elevation_sc), 
        max(thrush_sf$elevation_sc), 
        length = 25))
  
  
  cgram_elev_1 =
    ff_correlogram(
      st_coordinates(thrush_sf),
      predict(fit_elev_poly_sc, newdata))
}



# ---- correlogram_quadratic ----
{
  cgram_resid_1 =
    ff_correlogram(
      st_coordinates(thrush_sf),
      residuals(fit_elev_poly_sc)
    )
}



# ---- correlogram_spaced ----
{
  
  cgram_resid_spatial =
    ff_correlogram(
      st_coordinates(dat_2),
      residuals(fit_spaced)
    )
}




# ---- correlogram_plots ----
{
  gg_correlogram_elev = plot_ff_correlogram(cgram_elev_1, "Correlogram of Response")
  gg_correlogram_resid_1 = plot_ff_correlogram(cgram_resid_1, "Correlogram of Residuals", subttl = "Aspatial Model")
  gg_correlogram_resid_spaced = plot_ff_correlogram(cgram_resid_spatial, "Correlogram of Residuals", subttl = "Spaced-Sample Model")
  # gg_correlogram_elev  
  # gg_correlogram_resid_1
  # gg_correlogram_resid_spaced
}



