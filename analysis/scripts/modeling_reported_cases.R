################
# Data loading #
################

data_path <- file.path('_data/') # Data directory path
file_name <- 'dt_final.csv' # csv. file name 
file_path <- file.path(data_path, file_name) # Full csv. file path

library(tidyverse)

# Column names
col_names <- c(
  'district',
  'year',
  'month',
  'falciparum',
  'vivax',
  'aet',
  'prcp',
  'q',
  'soilm',
  'tmax',
  'tmin',
  'water_deficit',
  'loss',
  'loss_km2',
  'cum_loss_km2',
  'diag',
  'enviro',
  'nets',
  'workers',
  'pamafro',
  'pop2015',
  'province',
  'region',
  'id_district'
)

# Column types
col_types <- readr::cols(
  district = col_character(),
  year = col_integer(),
  month = col_integer(),
  falciparum = col_integer(),
  vivax = col_integer(),
  aet = col_double(),
  prcp = col_double(),
  q = col_double(),
  soilm = col_double(),
  tmax = col_double(),
  tmin = col_double(),
  water_deficit = col_double(),
  loss = col_double(),
  loss_km2 = col_double(),
  cum_loss_km2 = col_double(),
  diag = col_integer(),
  enviro = col_integer(),
  nets = col_integer(),
  workers = col_integer(),
  pamafro = col_character(),
  pop2015 = col_integer(),
  province = col_character(),
  region = col_character(),
  id_district = col_integer()
)

# Read raw csv. file into a data frame
raw_dataset <- 
  readr::read_csv(
    file = file_path,
    col_names = col_names,
    col_types = col_types,
    skip = 1,
    locale = readr::locale(encoding = "utf-8")
  )

# Create columns
# - report_date: 
# Date of cases report in date-time format
# - control: 
# Flag column indicating if the PAMAFRO intervention was taken place
# in that date
raw_dataset %>% 
  dplyr::mutate(
    report_date = 
      lubridate::make_datetime(
        year = year,
        month = month,
        day = 1L # Take 01 as the day
      ) 
  ) %>% 
  dplyr::mutate(
    control = 
      ifelse(
        test = report_date < '2005-09-01',
        yes = 1,
        no = ifelse(
          test = report_date < '2010-08-01',
          yes = 2,
          no = 3
        )
      )
  ) %>% 
  dplyr::arrange(report_date) -> 
  dataset # Final data frame

theme_set(theme_linedraw() + 
            theme(
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black")
            )
)

library(GGally)

dataset %>%
  select(
    falciparum,
    vivax,
    aet,
    prcp,
    q,
    soilm,
    tmax,
    tmin,
    water_deficit,
    loss_km2
  ) %>% 
  GGally::ggpairs(
    progress = FALSE
  )


control_dates <- 
  dplyr::tibble(
    dates = c(
      lubridate::make_datetime(
        year = 2005,
        month = 10,
        day = 1L
      ),
      lubridate::make_datetime(
        year = 2010,
        month = 09,
        day = 1L
      )
    ),
    type = c(
      'start',
      'end'
    )
  )

# Get static correlations
get_static_correlations <- function(data, cols){
  library(tibble, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  
  # Takes the correlation of all the variables in cols w.r.t. the first one
  # in the vector
  cor_matrix <- stats::cor(
    x = data[cols]
  )
  
  cor_vector <- matrix(
    data = cor_matrix[1,],
    nrow = 1,
    dimnames = 
      list(
        NULL,
        names(cor_matrix[1,])
      )
  )
  
  df_correlations <- tibble::as_tibble(
    x = cor_vector
  )
  
  df_correlations_pivot <- 
    tidyr::pivot_longer(
      data = df_correlations,
      cols = cols[2:length(cols)],
      names_to = 'variable',
      values_to = 'value'
    )
  
  return(df_correlations_pivot)
}

# Get correlations for a given window
get_correlations <- function(data, cols, index){
  library(lubridate, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  
  # Takes the correlation of all the variables in cols w.r.t. the first one
  # in the vector
  cor_matrix <- stats::cor(
    x = data[cols]
  )
  
  cor_vector <- matrix(
    data = cor_matrix[1,],
    nrow = 1,
    dimnames = 
      list(
        NULL,
        names(cor_matrix[1,])
      )
  )
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  df_correlations <- tibble::tibble(
    from = min(date),
    to = max(date)
  )
  
  df_correlations <- tibble::add_column(
    .data = df_correlations,
    tibble::as_tibble(cor_vector)
  )
  
  return(df_correlations)
}

# Get rolling correlations
rolling_correlation <- function(data, cols, index, window) {
  library(slider)
  library(lubridate, warn.conflicts = FALSE)
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  correlations <- slider::slide_period_dfr(
    .x = data,
    .i = date,
    .period = 'month',
    .f = get_correlations,
    cols = cols,
    index = index,
    .every = 1,
    .before = window - 1,
    .complete = TRUE
  )
  
  return(correlations)
}

# Get plots for rolling correlations
plot_rolling_cor <- function(data, cor_data, title){
  library(ggplot2, warn.conflicts = FALSE)
  
  plot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = to,
      y = value
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(
      date_labels = '%Y',
      date_breaks = '1 year'
    ) +
    ggplot2::geom_vline(
      data = control_dates,
      ggplot2::aes(
        xintercept = as.numeric(dates)
      ),
      linetype = 'dashed',
      colour = 'red'
    ) +
    ggplot2::geom_hline(
      data = cor_data,
      ggplot2::aes(
        yintercept = value
      ),
      linetype = 'dashed',
      colour = 'blue'
    ) +
    ggplot2::geom_hline(
      data = control_dates,
      ggplot2::aes(
        yintercept = 0
      ),
      linetype = 'dashed',
      alpha = 0.6
    ) +
    ggplot2::facet_wrap(
      ncol = 2,
      nrow = 4,
      facets = . ~ variable,
      scales = 'free'
    ) +
    ggplot2::labs(
      y = NULL, 
      x = NULL
    ) +
    ggplot2::ggtitle(
      label = title
    )+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
}

cor_vivax_region <- get_static_correlations(
  data = dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  )
)

roll_cor_12_vivax_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 12
)

# Plot
roll_cor_12_vivax_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_vivax_region,
    title = 'Rolling correlations (w = 12 months) with number of reported cases (vivax)'
  )

roll_cor_18_vivax_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 18
)

# Plot
roll_cor_18_vivax_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_vivax_region,
    title = 'Rolling correlations (w = 18 months) with number of reported cases (vivax)'
  )

roll_cor_24_vivax_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 24
)

# Plot
roll_cor_24_vivax_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_vivax_region,
    title = 'Rolling correlations (w = 24 months) with number of reported cases (vivax)'
  )

cor_falciparum_region <- get_static_correlations(
  data = dataset,
  cols = c(
    'falciparum',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  )
)

roll_cor_12_falciparum_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'falciparum',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 12
)

# Plot
roll_cor_12_falciparum_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_falciparum_region,
    title = 'Rolling correlations (w = 12 months) with number of reported cases (falciparum)'
  )

roll_cor_18_falciparum_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'falciparum',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 18
)

# Plot
roll_cor_18_falciparum_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_falciparum_region,
    title = 'Rolling correlations (w = 18 months) with number of reported cases (falciparum)'
  )

roll_cor_24_falciparum_region <- rolling_correlation(
  data = dataset,
  cols = c(
    'falciparum',
    'aet',
    'prcp',
    'q',
    'soilm',
    'tmax',
    'tmin',
    'water_deficit',
    'loss_km2'
  ),
  index = 'report_date',
  window = 24
)

# Plot
roll_cor_24_falciparum_region %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      q,
      soilm,
      tmax,
      tmin,
      water_deficit,
      loss_km2
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  plot_rolling_cor(
    cor_data = cor_falciparum_region,
    title = 'Rolling correlations (w = 24 months) with number of reported cases (falciparum)'
  )

# Get coefficients for a given window
get_coefficients <- function(data, formula, index){
  library(MASS)
  library(lubridate, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  
  model <- MASS::glm.nb(
    formula = formula,
    data = data
  )
  
  coefficients <- model$coefficients
  n_coefficients <- length(coefficients)
  
  coefficients_vector <- matrix(
    data = coefficients,
    nrow = 1,
    ncol = n_coefficients
  )
  
  names_coefficients <- names(coefficients)
  names_coefficients[1] <- 'intercept'
  dimnames(coefficients_vector) <- list(
    NULL,
    names_coefficients
  )
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  df_coefficients <- tibble::tibble(
    from = min(date),
    to = max(date)
  )
  
  df_coefficients <- tibble::add_column(
    .data = df_coefficients,
    tibble::as_tibble(coefficients_vector)
  )
  
  return(df_coefficients)
}

# Get coefficients' confidence interval lower bounds
get_coef_confint_lower <- function(data, formula, index){
  library(MASS)
  library(lubridate, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  
  model <- MASS::glm.nb(
    formula = formula,
    data = data
  )
  
  lower_bounds <- stats::confint(model)[,1]
  n_coefficients <- length(lower_bounds)
  
  lower_bounds_vector <- matrix(
    data = lower_bounds,
    nrow = 1,
    ncol = n_coefficients
  )
  
  names_coefficients <- names(lower_bounds)
  names_coefficients[1] <- 'intercept'
  dimnames(lower_bounds_vector) <- list(
    NULL,
    names_coefficients
  )
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  df_lower_bounds <- tibble::tibble(
    from = min(date),
    to = max(date)
  )
  
  df_lower_bounds <- tibble::add_column(
    .data = df_lower_bounds,
    tibble::as_tibble(lower_bounds_vector)
  )
  
  return(df_lower_bounds)
}

# Get coefficients' confidence interval upper bounds
get_coef_confint_upper <- function(data, formula, index){
  library(MASS)
  library(lubridate, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  
  model <- MASS::glm.nb(
    formula = formula,
    data = data
  )
  
  upper_bounds <- stats::confint(model)[,2]
  n_coefficients <- length(upper_bounds)
  
  upper_bounds_vector <- matrix(
    data = upper_bounds,
    nrow = 1,
    ncol = n_coefficients
  )
  
  names_coefficients <- names(upper_bounds)
  names_coefficients[1] <- 'intercept'
  dimnames(upper_bounds_vector) <- list(
    NULL,
    names_coefficients
  )
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  df_upper_bounds <- tibble::tibble(
    from = min(date),
    to = max(date)
  )
  
  df_upper_bounds <- tibble::add_column(
    .data = df_upper_bounds,
    tibble::as_tibble(upper_bounds_vector)
  )
  
  return(df_upper_bounds)
}

# Get rolling coefficients
rolling_nb_regression <- function(data, formula, index, window) {
  library(slider)
  library(lubridate, warn.conflicts = FALSE)
  
  date <- lubridate::as_datetime(
    x = as.matrix(data[index])
  )
  
  coefficients <- slider::slide_period_dfr(
    .x = data,
    .i = date,
    .period = 'month',
    .f = get_coefficients,
    formula = formula,
    index = index,
    .every = 1,
    .before = window - 1,
    .complete = TRUE
  )
  
  lower_bounds <- slider::slide_period_dfr(
    .x = data,
    .i = date,
    .period = 'month',
    .f = get_coef_confint_lower,
    formula = formula,
    index = index,
    .every = 1,
    .before = window - 1,
    .complete = TRUE
  )
  
  upper_bounds <- slider::slide_period_dfr(
    .x = data,
    .i = date,
    .period = 'month',
    .f = get_coef_confint_upper,
    formula = formula,
    index = index,
    .every = 1,
    .before = window - 1,
    .complete = TRUE
  )
  
  return(
    list(
      coefficients = coefficients,
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds
    )
  )
}

pivot_output <- function(data, value){
  library(tidyr, warn.conflicts = FALSE)
  
  df <- tidyr::pivot_longer(
    data = data,
    cols = c(
      intercept,
      soilm,
      loss_km2,
      aet,
      water_deficit
    ),
    names_to = 'variable',
    values_to = value
  )
  
  return(df)
}

# Rolling regression plot
plot_rolling_regre <- function(data, title){
  library(dplyr, warn.conflicts = FALSE)
  library(ggplot2, warn.conflicts = FALSE)
  
  coefficients <- pivot_output(
    data = data$coefficients,
    value = 'coefficient'
  )
  
  lower_bounds <- pivot_output(
    data = data$lower_bounds,
    value = 'lb'
  )
  
  upper_bounds <- pivot_output(
    data = data$upper_bounds,
    value = 'ub'
  )
  
  coefficients %>% 
    dplyr::inner_join(
      y = lower_bounds,
      by = c('from', 'to', 'variable')
    ) %>% dplyr::inner_join(
      y = upper_bounds,
      by = c('from', 'to', 'variable')
    ) -> df
  
  plot <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = to,
      y = coefficient
    )
  ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = lb,
        ymax = ub
      ),
      fill = 'grey70',
      alpha = 0.8
    ) + 
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        y = coefficient
      )
    ) +
    ggplot2::scale_x_datetime(
      date_labels = '%Y',
      date_breaks = '1 year'
    ) +
    ggplot2::geom_vline(
      data = control_dates,
      ggplot2::aes(
        xintercept = as.numeric(dates)
      ),
      linetype = 'dashed',
      colour = 'red'
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(
        yintercept = 0
      ),
      linetype = 'dashed',
      alpha = 0.6
    ) +
    ggplot2::facet_wrap(
      ncol = 1,
      facets = . ~ variable,
      scales = 'free'
    ) +
    ggplot2::labs(
      y = NULL, 
      x = NULL
    )+
    ggplot2::ggtitle(
      label = title
    )+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
}

vivax_nb_model_formula <- stats::as.formula(
  vivax ~ soilm + loss_km2 + aet + water_deficit
)

roll_regre_12_vivax_region <- rolling_nb_regression(
  data = dataset,
  formula = vivax_nb_model_formula,
  index = 'report_date',
  window = 12
)

# Plot
roll_regre_12_vivax_region %>% 
  plot_rolling_regre()

roll_regre_18_vivax_region <- rolling_nb_regression(
  data = dataset,
  formula = vivax_nb_model_formula,
  index = 'report_date',
  window = 18
)

# Plot
roll_regre_18_vivax_region %>% 
  plot_rolling_regre(title = "Rolling regression coefficients (w = 18 months)")

roll_regre_24_vivax_region <- rolling_nb_regression(
  data = dataset,
  formula = vivax_nb_model_formula,
  index = 'report_date',
  window = 24
)

# Plot
roll_regre_24_vivax_region %>% 
  plot_rolling_regre(title = "Rolling regression coefficients (w = 24 months)")

#### GAM

dataset %>% 
  dplyr::mutate(
    t = as.numeric(report_date)
  ) -> dataset

gam_cross_validation <- function(data, formula, k, seed)
{
  library(mgcv, warn.conflicts = FALSE)
  
  rows <- nrow(data)
  set.seed(seed)
  data <- data[sample(rows),]
  folds <- cut(
    x = seq(1, rows), 
    breaks = k,
    labels = FALSE
  )
  
  cv_rmse <- c()
  cv_mae <- c()
  cv_cor <- c()
  
  for(i in 1:k)
  {
    id_test <- which(
      x = folds == i,
      arr.ind = TRUE
    )
    test <- data[id_test,]
    train <- data[-id_test,]
    
    model <- mgcv::gam(
      formula = formula,
      family = mgcv::nb(),
      data = train
    )
    
    prediction <- predict.gam(
      object = model,
      type = "response",
      newdata = test
    )
    
    prediction <- as.matrix(prediction, ncol = 1)
    
    actual = as.matrix(test[model$terms[[2]]], ncol = 1)
    
    cv_rmse[i] <- sqrt(mean((prediction - actual)^2))
    cv_mae[i] <- mean(abs(prediction - actual))
    cv_cor[i] <- cor(prediction, actual)
  }
  
  rmse <- mean(cv_rmse)
  mae <- mean(cv_mae)
  corr <- mean(cv_cor)
  
  results <- list(
    "rmse" = rmse,
    "mae" = mae,
    "cor" = corr
  )
  
  return(results)
}

gam_model_criteria <- function(formulas, data, k, seed)
{
  library(mgcv, warn.conflicts = FALSE)
  library(tibble)
  
  models <- list()
  aic <- c()
  rmse <- c()
  mae <- c()
  corr <- c()
  
  for(i in 1:length(formulas))
  {
    models[[i]] <- mgcv::gam(
      formula = formulas[[i]],
      family = nb(),
      data = data
    )
    
    aic[i] <- stats::AIC(models[[i]])
    
    cv_results <- gam_cross_validation(
      data = data,
      formula = formulas[[i]],
      k = k,
      seed = seed
    )
    
    rmse[i] <- cv_results$rmse
    mae[i] <- cv_results$mae
    corr[i] <- cv_results$cor
    
  }
  
  criteria <- tibble::tibble(
    id = 1:length(formulas),
    model = names(formulas),
    aic = aic,
    rmse = rmse,
    mae = mae,
    corr = corr
  )
  
  results <- list(
    "criteria" = criteria,
    "models" = models
  )
  return(results)
}

vivax_gam_models <- 
  list(
    "glm" = formula(
      vivax ~ 
        s(t, by = soilm) + 
        s(t, by = loss_km2) + 
        s(t, by = aet) + 
        s(t, by = water_deficit)
    ),
    "m1" = formula(
      vivax ~ 
        s(id_district) +
        s(t) +
        s(t, by = soilm) + 
        s(t, by = loss_km2) + 
        s(t, by = aet) + 
        s(t, by = water_deficit)
    )
  )

vivax_gam_criteria <- gam_model_criteria(
  formulas = vivax_gam_models,
  data = dataset,
  k = 5,
  seed = 123
)

vivax_gam_criteria

summary(vivax_gam_criteria$models[[1]])

vivax_smooth_pred <- mgcv::predict.gam(
  object = vivax_gam_criteria$models[[1]],
  type = "terms",
  se.fit = TRUE
)

vivax_fit <- vivax_smooth_pred$fit
vivax_model_matrix <- vivax_gam_criteria$models[[1]]$model[c(3, 4, 5, 6)]
vivax_fit_values <- vivax_fit / vivax_model_matrix
vivax_names <- colnames(vivax_model_matrix)

# vivax_se_fit <- vivax_smooth_pred$se.fit
# colnames(vivax_se_fit) <- vivax_names

tibble::tibble(
  report_date = dataset$report_date,
  tibble::as_tibble(vivax_fit_values)
) %>% 
  tidyr::pivot_longer(
    cols = c(
      soilm,
      loss_km2,
      aet
    ),
    names_to = 'variable',
    values_to = 'fit'
  ) -> df_vivax_fit_values

# tibble::tibble(
#   report_date = dataset$report_date,
#   tibble::as_tibble(vivax_se_fit)
# ) %>% 
#   tidyr::pivot_longer(
#     cols = c(
#       soilm,
#       loss_km2,
#       aet,
#       water_deficit
#     ),
#     names_to = 'variable',
#     values_to = 'se_fit'
#   ) -> df_vivax_se_fit
# 
# df_vivax_fit_values %>% 
#   dplyr::inner_join(
#     y = df_vivax_se_fit,
#     by = c('report_date', 'variable')
# ) -> df_vivax_gam

df_vivax_fit_values %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = report_date,
      y = fit
    )
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = fit
    )
  ) +
  # ggplot2::geom_ribbon(
  #   ggplot2::aes(
  #     ymin = fit - se_fit,
  #     ymax = fit + se_fit
  #   ),
  #   fill = 'grey70',
  #   alpha = 0.8
  # ) +
  ggplot2::scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  ggplot2::geom_vline(
    data = control_dates,
    ggplot2::aes(
      xintercept = as.numeric(dates)
    ),
    linetype = 'dashed',
    colour = 'red'
  ) +
  ggplot2::geom_hline(
    ggplot2::aes(
      yintercept = 0
    ),
    linetype = 'dashed',
    alpha = 0.6
  ) +
  ggplot2::facet_wrap(
    ncol = 1,
    facets = . ~ variable,
    scales = 'free'
  ) +
  ggplot2::labs(
    y = NULL, 
    x = NULL
  )
