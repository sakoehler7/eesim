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
#'           exposure_type = "continuous", average_outcome = 20, rr = 1.10,
#'           model = "spline", df_year = 1)
#' coverage_plot(ex_sim[[1]], true_param = 1.10)
#'
coverage_plot <- function(summarystats, true_param){
summarystats %>%
  arrange(Estimate) %>%
  mutate(index = 1:n(),
         rr = exp(Estimate),
         lower_rr = exp(lower_ci),
         upper_rr = exp(upper_ci),
         includes_true = lower_rr < 1.10 & 1.10 < upper_rr) %>%
  ggplot(aes(x = index, y = true_param, color = includes_true)) +
  coord_flip() +
  geom_point() +
  geom_errorbar(aes(ymin = lower_rr, ymax = upper_rr)) +
  geom_hline(yintercept = 1.10, linetype = 2) +
  scale_color_manual(values = c("red", "darkgray")) +
  theme(legend.position="none",
        panel.background = element_rect(fill='white', colour='white')) +
  ylab("Relative risk") +
  scale_x_discrete(breaks = NULL) + xlab("")
}
#'
#'
#'
#'
#'
