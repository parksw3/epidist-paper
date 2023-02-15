#' Calculate the cohort-based or cumulative mean
#' @export
calculate_cohort_mean <- function(data, type = c("cohort", "cumulative"),
                                  by = c()) {
  type <- match.arg(type)

  out <- data |>
    copy() |>
    DT(, .(
      mean = mean(delay_daily), n = .N),
      by = c("ptime_daily", by)
     ) |>
    DT(order(rank(ptime_daily)))

  if (type == "cumulative") {
    out[, mean := cumsum(mean * n)/cumsum(n), by = by]
    out[, n := cumsum(n), by = by]
  }

  return(out[])
}

#' Calculate the truncated mean by primary event time
#' @export
calculate_truncated_means <- function(draws, obs_at, ptime) {
  if (length(ptime) != 2) {
    stop("ptime must be a vector of length 2.")
  }
  pvec <- seq(ptime[1], ptime[2], by = 1)
  estmat <- matrix(NA, nrow = nrow(draws), ncol = length(pvec))

  ## calculate truncated mean
  message("Calculating truncated means...")
  for (j in seq_along(pvec)) {
    estvec <- rep(NA, nrow(draws))

    for (i in seq_len(nrow(draws))) {

      p <- pvec[j]

      numer <- integrate(
        function(x) {
          x * dlnorm(x, meanlog = draws$meanlog[i], sdlog = draws$sdlog[i])
        },
        lower = 0, upper = obs_at - p
      )[[1]]

      denom <- integrate(
        function(x) {
          dlnorm(x, meanlog = draws$meanlog[i], sdlog = draws$sdlog[i])
        },
        lower = 0, upper = obs_at - p
      )[[1]]

      estmat[i, j] <- numer / denom
    }
  }
  return(estmat)
}