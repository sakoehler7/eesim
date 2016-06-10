#' Fit a generalized linear model
#'
#' This function fits a GLM with splines to your data.
#'
#' @param df A data frame with exposure and outcome data
#' @param df_year A numeric value specifying the degrees of freedom per year
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in exposure
#'
#' @export
#'
spline_mod <- function(df, df_year = 7){
  require(splines)

  dgrs_free <- df_year * as.numeric(diff(df[c(1, nrow(df)), "date"])) / 365.4
  df$time <- scale(df$date, center = TRUE, scale = FALSE)
  mod <- glm(y ~ x + ns(time, round(dgrs_free)),
             data = df,
             family = quasipoisson(link = "log"))

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}
#'
#' Fit a case-crossover model
#'
#' This function fits a case-crossover model to your data.
#'
#' @inheritParams spline_mod
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in exposure
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
      mod <- glm(y ~ x + stratum,
                 data = df,
                 family = quasipoisson(link = "log"))
    } else {
      mod <- glm(y ~ x,
                 data = df,
                 family = quasipoisson(link = "log"))
    }
  } else {
    mod <- glm(y ~ x + stratum,
               data = df,
               family = quasipoisson(link = "log"))
  }

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
