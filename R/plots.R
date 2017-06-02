#' Plot coverage of empirical confidence intervals
#'
#' Plots the relative risk point estimates and their confidence
#' intervals for model fit results for each simulation, compared to the true
#' relative risk. This gives a visualization of the coverage of the specified
#' method for the relative risk. The confidence intervals which do not contain
#' the true relative risk appear in red. The input to this function should be
#' either the output of \code{\link{fit_mods}} or the second element of the
#' output of \code{\link{eesim}}.
#'
#' @param summarystats A list or data frame of summary statistics from many
#' repetitions of a simulation.  Must include columns titled \code{Estimate},
#' \code{lower_ci}, and \code{upper_ci}. This could be the second object from the output of
#' \code{\link{eesim}}, specified by using the format \code{eesim_output[[2]]}.
#' @param true_param The true value of the relative risk used to simulate the data.
#'
#' @return A plot displaying the coverage for the true value of the parameter by
#' the confidence intervals resulting from each repetition of the simulation.
#'
#' @examples
#' ex_sim <- eesim(n_reps = 100, n = 1000, central = 100, sd = 10,
#'           exposure_type = "continuous", average_outcome = 20, rr = 1.02,
#'           custom_model = spline_mod, custom_model_args = list(df_year = 1))
#' coverage_plot(ex_sim[[2]], true_param = 1.02)
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

#' Create calendar plot
#'
#' Creates a calendar plot of a time series of continuous or discrete data. The time series
#' data frame input to this function must have only two columns, one for the date and one with
#' the values to plot.
#'
#' @param df Data frame with one column named \code{date} for date with entries in the format
#' "yyyy-mm-dd" and one column for the daily values of the variable to plot.
#' @param type Character string specifying whether the exposure is continuous or
#' discrete
#' @param labels Vector of character strings naming the levels of a discrete
#' variable to be used in the figure legend.
#' @param legend_name Character string specifying the title to be used in the figure
#'    legend.
#'
#' @details The output of this function is a \code{ggplot} object, so you can customize
#'    this output object as with any \code{ggplot} object.
#'
#' @examples
#' testdat <- sim_exposure(n = 1000, central = 0.1,
#'            exposure_type = "binary")
#' testdat$x[c(89,101,367,500,502,598,678,700,895)] <- 3
#' calendar_plot(testdat, type = "discrete", labels = c("no", "yes", "maybe"))
#'
#' @importFrom dplyr %>%
#'
#' @export
calendar_plot <- function(df, type = "continuous", labels = NULL, legend_name = "Exposure"){
  names(df) <- c("date", "x")

  if(type == "continuous"){
    exposure <- df$x
  } else if(type == "discrete"){
    if(length(labels) == 0){
      labels <- as.character(1:length(levels(factor(df$x))))
      }
    exposure <- factor(df$x, levels = levels(factor(df$x)), labels = labels)
  } else {
    stop('The parameter `type` must be "continuous" or "discrete".')
  }

  plot <- df %>%
    dplyr::mutate_(Weekday = ~ lubridate::wday(date),
                  Month = ~ lubridate::month(date, label = TRUE),
                  Year = ~ lubridate::year(date),
                  Exposure = ~ exposure) %>%
    dplyr::group_by_(.dots = list("Year", "Month")) %>%
    dplyr::mutate_(saturday = ~ dplyr::lag(Weekday) == 7,
                  saturday = ~ ifelse(is.na(saturday), 0, saturday),
                  Week = ~ 1 + cumsum(saturday)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes_(x = ~ Weekday, y = ~ Week, fill = ~ Exposure)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::facet_grid(Year ~ Month, scales = "free")
  if(type=="continuous"){
    newplot <- plot + viridis::scale_fill_viridis(name = legend_name) +
      ggplot2::scale_y_reverse() + ggplot2::theme_void()
  } else {
    newplot <- plot +
      viridis::scale_fill_viridis(name = legend_name, discrete = TRUE, begin = 0.2, end = 0.9) +
      ggplot2::scale_y_reverse() + ggplot2::theme_void()
  }

  return(newplot)
}
#'
#'
#'
