#' Plot the posterior estimates as densities
#' @export
plot_recovery <- function(data, alpha = 0.8,
                                   quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  data |>
    ggplot() +
    aes(x = value, ...) +
    ggridges::geom_density_ridges(
      scale = 1.5, alpha = alpha, quantile_lines = TRUE,
      quantiles = quantiles
    ) +
    theme_bw()
}

#' Plot the relative difference between true values and posterior estimates
#' @export
plot_relative_recovery <- function(relative_data, alpha = 0.8,
                                   quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  relative_data |>
    plot_recovery(
      data = copy(relative_data)[, value := rel_value],
      alpha = alpha, quantiles = quantiles, ...
    ) +
    geom_vline(xintercept = 1, linetype = 2, size = 1.05, alpha = 0.8) +
    labs(x = "Relative value")
}

#' Plot cases by observation window
#' @export
plot_cases_by_obs_window <- function(cases) {
  cases |>
    DT(case_type == "primary") |>
    ggplot() +
    aes(x = time, y = cases) +
    geom_col(aes(fill = factor(obs_at)), alpha = 1, col = "#696767b1") +
    geom_point(
      data = cases[case_type == "secondary"],
      aes(col = factor(obs_at))
    ) +
    geom_vline(
      aes(xintercept = as.numeric(as.character(obs_at))),
      linetype = 2, alpha = 0.9
    ) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    scale_color_brewer(palette = "Reds", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Cases") + 
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE),
           color = guide_legend(title = "Observation day", reverse = TRUE))
}

#' Plot the empirical delay distribution
#' @export
plot_empirical_delay <- function(cases, meanlog, sdlog) {
  plot <- cases |>
    ggplot() +
    aes(x = delay_daily) +
    geom_histogram(
      aes(
        y = after_stat(density), fill = obs_at), binwidth = 1,
        position = "dodge", col = "#696767b1"
    )

  if (!missing(meanlog) && !missing(sdlog)) {
    plot <- plot +
      stat_function(
        fun = dlnorm, args = c(meanlog, sdlog), n = 100,
        col = "#696767b1"
      )
  }

  plot <- plot +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Density") +
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE))

  return(plot)
}

#' Plot the mean difference between continuous and discrete event time
#' @export
plot_censor_delay <- function(censor_delay) {
  ggplot(censor_delay) +
    geom_point(aes(cohort, mean))  +
    geom_errorbar(aes(cohort, ymin=lwr, ymax=upr), width=0) +
    facet_wrap(~type)  +
    theme_bw() +
    labs(
      x = "Cohort time (day)",
      y = "Mean different between continuous and discrete event time"
    )
}

#' plot empirical cohort-based or cumulative mean vs posterior mean
#' @param draws draws from posterior distribution for a model fit
#' @param data data used for object fitting
#' @param type type of mean to plot
#' @param truncate account for truncation?
#' @export
plot_posterior_pred_check <- function(draws,
                                      data,
                                      type = c("cohort", "cumulative"),
                                      truncate,
                                      obs_at) {
  type <- match.arg(type)

  gplot <- data |>
    calculate_cohort_mean(type) |>
    plot_cohort_mean()

  prange <- range(data$ptime_daily)
  pvec <- seq(prange[1], prange[2], by = 1)

  if (truncate) {
    if (type == "cumulative") {
      stop("Don't use the truncate option with cumulative mean.")
    }

    if (missing(obs_at)) {
      message("obs_at not specified. Using maximum secondary event time.")
      obs_at <- max(data$stime_daily)
    }
    estmat <- calculate_truncated_means(
      draws, obs_at = obs_at, ptime = prange
    )
    fitted <- data.table(
      pvec = pvec,
      mean = apply(estmat, 2, mean),
      lwr = apply(estmat, 2, quantile, 0.025),
      upr = apply(estmat, 2, quantile, 0.975)
    )
  } else {
    fitted <- data.table(
      pvec = pvec,
      mean = mean(ee$mean),
      lwr = quantile(ee$mean, 0.025),
      upr = quantile(ee$mean, 0.975)
    )
  }

  gplot <- gplot +
    geom_ribbon(
      data = fitted, aes(pvec, ymin = lwr, ymax = upr), alpha = 0.3,
      lty = 2, col = "black"
    ) +
    geom_line(data = fitted, aes(pvec, mean))

  return(gplot)
}

#' Plot empirical cohort-based or cumulative mean
#' @export
plot_cohort_mean <- function(data) {
  gplot <- ggplot(data) +
    geom_point(aes(x = ptime_daily, y = mean, size = n), shape = 1) +
    theme_bw() +
    scale_size("Number of samples") +
    labs(
      x = "Cohort time (day)",
      y = "Mean delay (days)"
    )
  return(gplot)
}