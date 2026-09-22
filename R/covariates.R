# Covariate-adjusted OLS and period-stratified influence functions.

covariate_design <- function(mf, day, built) {
  used_days <- sort(unique(c(day[day < 0], built$target)))
  keep <- day %in% used_days
  mf <- droplevels(as.data.frame(mf[keep, , drop = FALSE]))
  day <- day[keep]
  counts <- table(day)
  if (any(counts < 2L)) {
    stop("Covariate adjustment requires at least two complete responses in every pre-event and forecast period. Period(s): ",
      paste(names(counts)[counts < 2L], collapse = ", "), ".",
      call. = FALSE
    )
  }
  for (name in names(mf)[-(1:2)]) {
    value <- mf[[name]]
    if (is.character(value) || is.logical(value)) value <- factor(value)
    if (is.factor(value)) {
      value <- factor(value, ordered = FALSE)
      if (nlevels(value) < 2L) {
        stop("Covariate `", name, "` has fewer than two observed levels.", call. = FALSE)
      }
    } else if (!is.numeric(value) || !is.null(dim(value)) ||
      any(!is.finite(value))) {
      stop("Covariate `", name, "` must be finite numeric, logical, factor, or character.", call. = FALSE)
    }
    mf[[name]] <- value
  }
  # Use synthetic names so non-syntactic user column names remain valid.
  design_data <- mf[-1L]
  names(design_data) <- paste0("v", seq_along(design_data))
  design_data[[1L]] <- day
  design <- stats::model.matrix(~., data = design_data)
  # Scaling improves the QR solve without changing the fitted model space.
  for (j in 2:ncol(design)) {
    center <- mean(design[, j])
    scale <- stats::sd(design[, j])
    if (!is.finite(scale) || scale == 0) {
      stop("The covariate design contains a constant column; remove redundant covariates.", call. = FALSE)
    }
    design[, j] <- (design[, j] - center) / scale
  }
  list(x = design, y = mf[[1L]], day = day)
}

covariate_window <- function(data, fit_days, target, label) {
  train <- which(data$day %in% fit_days)
  x <- data$x[train, , drop = FALSE]
  model <- stats::lm.fit(x, data$y[train])
  if (model$rank != ncol(x)) {
    stop("The covariate model is rank deficient in ", label,
      ". Every fitting window must identify all coefficients; remove collinear covariates, check factor levels, or increase `window`.",
      call. = FALSE
    )
  }
  if (model$df.residual <= 0L) {
    stop("The covariate model has no residual degrees of freedom in ", label,
      "; reduce covariates or increase `window`.",
      call. = FALSE
    )
  }
  # The coefficient map is R^-1 Q', with QR pivot order restored.
  coefficient_map <- backsolve(qr.R(model$qr), t(qr.Q(model$qr)))
  coefficient_map <- coefficient_map[order(model$qr$pivot), , drop = FALSE]
  influence <- matrix(0, length(target), length(data$y))
  predictions <- errors <- numeric(length(target))
  for (j in seq_along(target)) {
    forecast <- which(data$day == target[j])
    zbar <- colMeans(data$x[forecast, , drop = FALSE])
    predictions[j] <- sum(zbar * model$coefficients)
    forecast_residual <- data$y[forecast] -
      as.numeric(data$x[forecast, , drop = FALSE] %*% model$coefficients)
    errors[j] <- mean(forecast_residual)
    influence[j, train] <- -as.numeric(zbar %*% coefficient_map) * model$residuals
    # This includes uncertainty in the forecast period's covariate mean,
    # including its covariance with the observed outcome mean.
    influence[j, forecast] <- forecast_residual / length(forecast)
  }
  # Condition on period counts, allowing independent respondents with
  # heterogeneous variances. Center scores within each sampling stratum.
  for (indices in split(seq_along(data$day), data$day)) {
    n <- length(indices)
    block <- influence[, indices, drop = FALSE]
    influence[, indices] <- (block - rowMeans(block)) * sqrt(n / (n - 1))
  }
  list(
    predictions = predictions, errors = errors, influence = influence,
    fit_rmse = sqrt(mean(model$residuals^2)),
    parameters = ncol(x), residual_df = model$df.residual,
    fitted = as.numeric(data$x %*% model$coefficients)
  )
}

covariate_interval <- function(point, variance, reference, reference_variance,
                               covariance, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  limits <- lapply(c(-1, 1), function(sign) {
    se <- sqrt(pmax(0, variance + reference_variance + 2 * sign * covariance))
    adjusted <- point + sign * reference
    c(low = min(adjusted - z * se), high = max(adjusted + z * se))
  })
  c(
    low = min(vapply(limits, `[`, numeric(1), 1L)),
    high = max(vapply(limits, `[`, numeric(1), 2L))
  )
}

covariate_days <- function(fitted, inference, reference, alpha) {
  intervals <- t(vapply(seq_len(nrow(fitted)), function(j) {
    covariate_interval(
      fitted$observed[j] - fitted$counterfactual[j],
      inference$day_covariance[j, j], reference$error,
      inference$reference_variance, inference$reference_day_covariance[, j], alpha
    )
  }, numeric(2)))
  data.frame(fitted,
    effect = fitted$observed - fitted$counterfactual,
    conf_low = intervals[, 1L], conf_high = intervals[, 2L]
  )
}

estimate_covariates <- function(mf, day, built, ps, alpha) {
  data <- covariate_design(mf, day, built)
  final <- covariate_window(data, built$final_fit, built$target, "the final fitting window")
  reference_variance <- numeric(nrow(built$reference_map))
  reference_day_covariance <- matrix(0, nrow(built$reference_map), length(built$target))
  reference_errors <- numeric(nrow(built$reference_map))
  rolling_rmse <- numeric(nrow(built$windows))
  for (j in seq_len(nrow(built$windows))) {
    win <- built$windows[j, ]
    indices <- which(built$reference_map$fit_start == win$fit_start)
    fit_days <- sort(unique(data$day[data$day >= win$fit_start & data$day <= win$fit_end]))
    fit <- covariate_window(
      data, fit_days,
      built$reference_map$forecast_period[indices],
      paste0("rolling fitting window [", win$fit_start, ", ", win$fit_end, "]")
    )
    # Accumulate only covariance blocks needed by the interval envelope.
    # This avoids retaining a references-by-respondents influence matrix.
    reference_variance[indices] <- rowSums(fit$influence^2)
    reference_day_covariance[indices, ] <- tcrossprod(fit$influence, final$influence)
    reference_errors[indices] <- fit$errors
    rolling_rmse[j] <- fit$fit_rmse
  }
  inference <- list(
    day_covariance = tcrossprod(final$influence),
    reference_variance = reference_variance,
    reference_day_covariance = reference_day_covariance
  )
  effect <- mean(final$errors)
  variance <- sum(colMeans(final$influence)^2)
  se <- sqrt(variance)
  ci <- covariate_interval(
    effect, variance, reference_errors,
    inference$reference_variance, rowMeans(inference$reference_day_covariance), alpha
  )
  z <- stats::qnorm(1 - alpha / 2)
  estimate <- data.frame(
    effect = effect, std_error = se,
    conventional_low = effect - z * se, conventional_high = effect + z * se,
    bias_bound = max(abs(reference_errors)), conf_low = unname(ci[1L]),
    conf_high = unname(ci[2L]), n_windows = built$n_windows,
    n_reference = length(reference_errors)
  )
  reference <- cbind(built$reference_map, error = reference_errors)
  fitted <- data.frame(
    relative_period = built$target,
    observed = ps$mean[match(built$target, ps$relative_period)],
    counterfactual = final$predictions
  )
  line_days <- sort(c(built$final_fit, built$target))
  line <- data.frame(
    relative_period = line_days,
    counterfactual = vapply(line_days, function(t) mean(final$fitted[data$day == t]), numeric(1))
  )
  list(
    estimate = estimate, reference = reference, fitted = fitted,
    days = covariate_days(fitted, inference, reference, alpha),
    inference = inference, counterfactual_path = line,
    model_diagnostics = list(
      parameters = final$parameters,
      final_residual_df = final$residual_df, final_fit_rmse = final$fit_rmse,
      rolling_fit_rmse = rolling_rmse
    )
  )
}
