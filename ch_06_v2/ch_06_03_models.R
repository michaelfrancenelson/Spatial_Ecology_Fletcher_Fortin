## ---- model_1 ----
  fit_elev = glm(
    VATH ~ elevation, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev)


## ---- model_2 ----
  fit_terrain = glm(
    VATH ~ elevation + slope + aspect, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_terrain)



## ---- model_3 ----
  fit_elev_poly =  glm(
    VATH ~ elevation + I(elevation^2), 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_poly)


## ---- model_1_center ----
  fit_elev_cen = glm(
    VATH ~ elevation_cen, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_cen)


## ---- model_2_center ----
  fit_terrain_cen = glm(
    VATH ~ elevation_cen + slope_cen + aspect_cen, 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_terrain_cen)



## ---- model_3_center ----
  fit_elev_poly_cen =  glm(
    VATH ~ elevation_cen + I(elevation_cen^2), 
    family = "binomial", 
    data = thrush_sf)
  summary(fit_elev_poly_cen)




## ---- model_diagnostics_1 ----
  plot(fit_elev)
  
  fit_elev$predicted = predict(fit_elev, type="response")
  fit_elev$residuals = residuals(fit_elev, type = "response")
  
  d$predicted <- predict(fit, type="response")
  d$residuals <- residuals(fit, type = "response")
  
  
  d$predicted <- predict(fit, type="response")
  d$residuals <- residuals(fit, type = "response")
  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()  
  # Steps 3 and 4: plot the results
  ggplot(d, aes(x = hp, y = vs)) +
    geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  thrush_sf$predicted = predict(fit_elev, type="response")
  thrush_sf$residuals = residuals(fit_elev, type = "response")
  
  # Steps 3 and 4: plot the results
  ggplot(thrush_sf, aes(x = elevation_cen, y = VATH)) +
    geom_segment(aes(xend = elevation_cen, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  fit_elev$predicted
  
}

## ---- model_1_diagnostics ----
{
  dat_fit_1 = data.frame(thrush_sf)
  dat_fit_1$predicted = predict(fit_elev_cen, type="response")
  dat_fit_1$residuals = residuals(fit_elev_cen, type="response")
  
  # plot residuals against elevation (the predictor)
  plot(residuals ~ elevation, data = dat_fit_1) 
  
  # Plot residuals against the predicted values
  plot(residuals ~ predicted, data = dat_fit_1) 
}


## ---- model_3_diagnostics ----
{
  dat_fit_3 = data.frame(thrush_sf)
  dat_fit_3$predicted = predict(fit_elev_poly_cen, type="response")
  dat_fit_3$residuals = residuals(fit_elev_poly_cen, type="response")  
  # plot residuals against elevation (the predictor)
  
  plot(residuals ~ elevation, data = dat_fit_3) 
  
  # Plot residuals against the predicted values
  plot(residuals ~ predicted, data = dat_fit_3, pch = 16, cex = 0.1)
} 

## ---- model_1_diagnostics_fancy ----
if (FALSE)
{
  
  with(subset(dat_fit_1, VATH == 0), lines(lowess(elevation, residuals)))
  with(subset(dat_fit_1, VATH == 1), lines(lowess(elevation, residuals)))
  with(dat_fit_1, lines(lowess(elevation, residuals)))
  
  plot(residuals ~ predicted, data = dat_fit_1, type = ) 
  # with(subset(dat_fit_1, VATH == 0), lines(lowess(predicted, residuals)))
  # with(subset(dat_fit_1, VATH == 1), lines(lowess(predicted, residuals)))
  with(dat_fit_1, lines(lowess(predicted, residuals)))
  
  
  plot(predicted ~ elevation, data = dat_fit_1) 
  ggplot(dat_fit_1, aes(x = elevation, y = residuals)) +
    # geom_segment(aes(xend = elevation, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  # Steps 3 and 4: plot the results
  ggplot(dat_fit_1, aes(x = predicted, y = residuals)) +
    # geom_segment(aes(xend = predicted, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    # geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  
  
}





## ---- model_3_diagnostics_fancy ----
if (FALSE)
{
  ggplot(dat_fit_1, aes(x = elevation, y = VATH)) +
    geom_segment(aes(xend = elevation, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
  plot(residuals ~ predicted, data = dat_fit_3)   
}





# ---- model_selection ----
{
  AIC(fit_elev_cen, fit_terrain_cen, fit_elev_poly_cen)
}





# ---- model_plots ----
{
  newdata = data.frame(
    elevation_cen =   
      seq(
        min(thrush_sf$elevation_cen), 
        max(thrush_sf$elevation_cen), 
        length = 25))
  
  # Use model to predict values for different elevations:
  # type = response for predicted probabilities
  glm.pred = predict(
    fit_elev_cen, 
    newdata = newdata, 
    type =  "link", 
    se = T) 
}