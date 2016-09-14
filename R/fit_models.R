#' Fit a generalized linear model
#'
#' This function fits a GLM with splines to your data.
#'
#' @param df A data frame with exposure and outcome data
#' @param df_year A numeric value specifying the degrees of freedom per year
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in
#'    exposure
#'
#' @examples
#' exp <- sim_exposure(n = 500, central = 100, sd = 10, trend = "cos1",
#'                     amp = .6, exposure_type = "continuous")
#' out <- sim_outcome(exposure = exp, average_outcome = 22, rr = 1.01)
#' spline_mod(df = out)
#'
#' @export
#'
spline_mod <- function(df, df_year = 7){
  dgrs_free <- df_year * as.numeric(diff(df[c(1, nrow(df)), "date"])) / 365.4
  df$time <- scale(df$date, center = TRUE, scale = FALSE)
  mod <- stats::glm(outcome ~ x + splines::ns(time, round(dgrs_free)),
                   data = df,
                   family = stats::quasipoisson(link = "log"))

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- stats::confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}

#' Fit a case-crossover model
#'
#' This function fits a case-crossover model to your data.
#'
#' @inheritParams spline_mod
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in
#'    exposure
#'
#' @examples
#' casecross_mod(df = out)
#'
#' @export
#'
casecross_mod <- function(df){
  df$stratum <- factor(format(df$date, "%Y.%m"))

  if (sum(df$x == 0 | df$x == 1) == length(df$x)){
    event.check <- as.matrix(table(df$stratum, df$x))
    informative.strata <- rownames(event.check)[apply(event.check,
                                                      1, prod) > 0]
    df <- subset(df, stratum %in% informative.strata)

    if(length(informative.strata) > 1){
      mod <- stats::glm(outcome ~ x + stratum,
                 data = df,
                 family = stats::quasipoisson(link = "log"))
    } else {
      mod <- stats::glm(outcome ~ x,
                 data = df,
                 family = stats::quasipoisson(link = "log"))
    }
  } else {
    mod <- stats::glm(outcome ~ x + stratum,
               data = df,
               family = stats::quasipoisson(link = "log"))
  }

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- stats::confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}

#' Fit a cross-year model
#'
#' This function fits a cross-year model to your data.
#'
#' @inheritParams spline_mod
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in
#'    exposure
#'
#' @examples
#' crossyear_mod(df = out)
#'
#' @export
#'
crossyear_mod <- function(df){
  df$stratum <- factor(format(df$date, "%j"))
  df$year <- factor(format(df$date, "%Y"))

  if (sum(df$x == 0 | df$x == 1) == length(df$x)){
    event.check <- as.matrix(table(df$stratum, df$x))
    informative.strata <- rownames(event.check)[apply(event.check,
                                                      1, prod) > 0]
    df <- subset(df, stratum %in% informative.strata)

    if(length(informative.strata) > 1){
      mod <- stats::glm(y ~ x + stratum,
                 data = df,
                 family = stats::quasipoisson(link = "log"))
    } else {
      mod <- stats::glm(y ~ x,
                 data = df,
                 family = stats::quasipoisson(link = "log"))
    }
  } else {
    mod <- stats::glm(y ~ x + stratum,
               data = df,
               family = stats::quasipoisson(link = "log"))
  }

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}
