#' Coverage Plot
#'
#' This plot displays the relative risk point estimates and their confidence
#' intervals for each repitition of the simulation in relation to the true
#' relative risk.  It is a visualization of the coverage of the specified
#' method for the relative risk. The confidence intervals which do not contain
#' the true relative risk appear in red.
#'
#' @param summarystats A list or data frame of summary statistics from many
#' repitions of a simulation.  Must include columns titled "Estimate",
#' "lower_ci", and "upper_ci". This could be the first object from the output of
#' eesim, specified by using the format "eesim_output[[1]]".
#' @param true_param The true value of the parameter being estimated
#'
#' @return A plot displaying the coverage for the true value of the parameter by
#' the confidence intervals resulting from each repitition of the simulation.
#'
#' @examples
#' ex_sim <- eesim(n_reps = 100, n = 10, central = 100, sd = 10,
#'           exposure_type = "continuous", average_outcome = 20, rr = 1.05,
#'           model = "spline", df_year = 1)
#' coverage_plot(ex_sim[[1]], true_param = 1.05)
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
coverage_plot <- function(summarystats, true_param){
  out <- summarystats %>%
    dplyr::arrange_(~ Estimate) %>%
    dplyr::mutate_(index = ~ 1:length(Estimate),
                   rr = ~ exp(Estimate),
                   lower_rr = ~ exp(lower_ci),
                   upper_rr = ~ exp(upper_ci),
                   includes_true = ~ lower_rr < true_param & true_param < upper_rr) %>%
    ggplot2::ggplot(ggplot2::aes_(x = ~ index, y = ~ rr, color = ~ includes_true)) +
    ggplot2::coord_flip() +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes_(ymin = ~ lower_rr, ymax = ~ upper_rr)) +
    ggplot2::geom_hline(yintercept = true_param, linetype = 2) +
    ggplot2::scale_color_manual(values = c("red", "darkgray")) +
    ggplot2::theme(legend.position="none",
                   panel.background = ggplot2::element_rect(fill = 'white', colour = 'white')) +
    ggplot2::ylab("Relative risk") +
    ggplot2::scale_x_discrete(name = "", breaks = NULL)
  return(out)
}
#'
#' Calendar Plot
#'
#' This plot displays daily values of a binary or continuous variable on a
#' calendar via color.
#'
#' @param df Data frame with one column for date with entries in the format
#' "yyyy-mm-dd" and one column for the daily values of the variable.
#' @param type Character string specifying whether the exposure is continuous or
#' discrete
#' @param labels Vector of character strings naming the levels of a discrete
#' variable
#'
#' @examples
#' testdat <- sim_exposure(n = 1000, central = 0.1,
#'            exposure_type = "binary")
#' testdat$x[c(89,101,367,500,502,598,678,700,895)] <- 3
#' calendar_plot(testdat, type = "discrete", labels = c("no", "yes", "maybe"))
#'
#'
calendar_plot <- function(df, type = "continuous", labels = NULL){
  names(df) <- c("date", "x")
  if(type=="continuous"){
    Exposure<- df$x
  }
  else if(type=="discrete"){
    Exposure <- factor(df$x, levels = levels(factor(df$x)), labels = labels)
  }
  plot <- df %>%
  mutate(Weekday = lubridate::wday(date),
         Month = lubridate::month(date, label = TRUE),
         Year = lubridate::year(date),
         Exposure) %>%
  group_by(Year, Month) %>%
  dplyr::mutate(saturday = lag(Weekday) == 7,
                saturday = ifelse(is.na(saturday), 0, saturday),
                Week = 1 + cumsum(saturday)) %>%
  ungroup() %>%
  ggplot(aes(x = Weekday, y = Week, fill = Exposure)) +
  geom_tile(colour = "white") +
  facet_grid(Year ~ Month, scales = "free")
  if(type=="continuous"){
    newplot <- plot + scale_fill_gradientn(colours = viridis(256)) +
      scale_y_reverse() + theme_void()
  }
  else if(type=="discrete"){
    newplot <- plot + viridis::scale_color_viridis(discrete=TRUE) +
      scale_y_reverse() + theme_void()
  }
  return(newplot)
}
#'
#'
#'
