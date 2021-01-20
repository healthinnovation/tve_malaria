data_path <- file.path('_data/') # Ruta del directorio de datos
file_name <- 'dt_final.csv' # Nombre del archivo
file_path <- file.path(data_path, file_name) # Ruta del archivo

#################
library(tidyverse)

# Nombre de las columnas
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

# Tipos de dato de las columnas
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
  pamafro = col_integer(),
  pop2015 = col_integer(),
  province = col_character(),
  region = col_character(),
  id_district = col_character()
)

raw_dataset <- 
  readr::read_csv(
    file = file_path,
    col_names = col_names,
    col_types = col_types,
    skip = 1,
    locale = readr::locale(encoding = "utf-8")
  )

raw_dataset %>% 
  dplyr::mutate(
    report_date = 
      lubridate::make_datetime(
        year = year,
        month = month,
        day = 1L
      )
  ) %>% 
  dplyr::mutate(
    control = 
      ifelse(
        test = (report_date >= '2005-09-01' & report_date <= '2010-09-01'),
        yes = 1,
        no = 0
      )
  ) %>% 
  dplyr::arrange(report_date) -> 
  dataset

#- Histograms

theme_set(theme_light())

dataset %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      soilm,
      tmax,
      vivax,
      falciparum
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
    aes(
      x = value
    )
  ) +
  ggplot2::geom_histogram(
    bins = 30,
    color = 'black',
    fill = 'white'
  ) +
  ggplot2::facet_wrap(
    facets = . ~ variable,
    scales = 'free'
  )

dataset %>% 
  select(
    aet,
    prcp,
    soilm,
    tmax,
    vivax,
    falciparum
  ) %>% 
  summary()

# Region

dataset %>%
  dplyr::group_by(
    region, report_date
  ) %>% 
  dplyr::summarise(
    aet = stats::median(aet),
    prcp = stats::median(prcp),
    soilm = stats::median(soilm),
    tmax = stats::median(tmax),
    vivax = sum(vivax),
    falciparum = sum(falciparum)
  ) -> region_dataset

region_dataset %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      soilm,
      tmax,
      vivax,
      falciparum
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
    aes(
      x = value
    )
  ) +
  ggplot2::geom_histogram(
    bins = 30,
    color = 'black',
    fill = 'white'
  ) +
  ggplot2::facet_wrap(
    facets = . ~ variable,
    scales = 'free'
  )

summary(region_dataset)

#- Correlation matrix

library(GGally)

region_dataset %>%
  GGally::ggpairs(
    columns = 3:8
  ) +
  ggtitle(
    label = 'Matriz de correlación del número de casos reportados y valores de
    variables climáticas mensuales durante los años 2005 a 2017
    en Loreto'
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#- Model

#-- Vivax
library(MASS)

full_model <- stats::as.formula(vivax ~ aet + prcp + soilm + tmax)

poisson_full_model <- stats::glm(
  formula = full_model,
  family = 'poisson',
  data = region_dataset
)

qpoisson_full_model <- stats::glm(
  formula = full_model,
  family = 'quasipoisson',
  data = region_dataset
)

nb_full_model <- MASS::glm.nb(
  formula = full_model,
  data = region_dataset
)

summary(poisson_full_model)
summary(qpoisson_full_model)
summary(nb_full_model)

colnames(stats::confint(nb_full_model))

stats::confint(nb_full_model)[,1]

nb_model <- MASS::glm.nb(
  formula = vivax ~ tmax,
  data = region_dataset
)

nb_full_model <- MASS::glm.nb(
  formula = vivax ~ tmax + aet + prcp + soilm,
  data = region_dataset
)

AIC(nb_model)
summary(ng_model)

actual_vs_fitted_plot = function(actual, fitted, title){
  library(dplyr)
  library(ggplot2)
  
  dplyr::tibble(
    actual = actual,
    fitted = fitted
  ) %>% 
    ggplot2::ggplot(
      aes(
        x = fitted,
        y = actual
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(
      slope = 1,
      colour = 'red',
      linetype = 'dashed'
    ) +
    ggplot2::ggtitle(
      label = title
    )
}

actual_vs_fitted_plot(
  actual = region_dataset$vivax,
  fitted = nb_model$fitted.values,
  title = 'NB'
)

ng_model$coefficients

#####################

#-- GAM

library(mgcv)

region_dataset %>% 
  dplyr::mutate(
    t = as.numeric(report_date)
  ) -> region_dataset

report_date <- as.numeric(
  x = region_dataset$report_date
)

nb_gam_model <- mgcv::gam(
  formula = vivax ~ s(t, by = tmax),
  family = nb(),
  data = region_dataset
)

summary(nb_gam_model)
nb_gam_model$coefficients
nb_gam_model$full.sp

actual_vs_fitted_plot(
  actual = region_dataset$vivax,
  fitted = nb_gam_model$fitted.values,
  title = 'NB GAM'
)

AIC(nb_gam_model, nb_model)

mgcv::plot.gam(nb_gam_model)
mgcv::vis.gam(nb_gam_model)

nb_gam_plot <- mgcv::plot.gam(nb_gam_model)

######################

nb_gam_full <- mgcv::gam(
  formula = vivax ~ 
    s(t, by = tmax) + 
    s(t, by = aet) + 
    s(t, by = prcp) + 
    s(t, by = soilm),
  family = nb(),
  data = region_dataset
)

nb_gam_full_plot <- mgcv::plot.gam(
  x = nb_gam_full_model,
  pages = 1,
  n = 216
)

mgcv::plot.gam(
  x = nb_gam_full_model,
  pages = 0,
  n = 216,
  select = 1
)

AIC(nb_gam_full)

actual_vs_fitted_plot(
  actual = region_dataset$vivax,
  fitted = nb_gam_full$fitted.values,
  title = 'NB GAM'
)

nb_gam_full_s_predictions <- mgcv::predict.gam(
  object = nb_gam_full,
  type = 'terms',
  se.fit = TRUE
)

plot(nb_gam_full_s_predictions$fit[,1] / region_dataset$tmax, type = 'l')

nb_gam_full$model

get_gam_smooth_values <- function(model, index){
  library(mgcv, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  library(lubridate, warn.conflicts = FALSE)
  
  smooth_pred <- mgcv::predict.gam(
    object = nb_gam_full,
    type = 'terms',
    se.fit = TRUE
  )
  
  smooth_values <- smooth_pred$fit
  smooth_se <- smooth_pred$se.fit
  
  df_smooth_values <- tibble::tibble(
    date = lubridate::as_datetime(
      x = model$model$index, 
      origin = lubridate::origin
    )
  )
  
  df_smooth_values <- tibble::add_column(
    .data = df_smooth_values,
    tibble::as_tibble(
      x = smooth_values
    )
  )
  
}

-6.533645 / 31.20974

#####################
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

dplyr::tibble(
  t = lubridate::as_datetime(
    x = nb_gam_full_plot[[4]]$x, 
    origin = lubridate::origin
  ),
  tmax_coeff = nb_gam_full_plot[[4]]$fit
) %>% 
  ggplot2::ggplot(
    aes(
      x = t,
      y = tmax_coeff
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(
    data = control_dates,
    aes(
      xintercept = as.numeric(dates)
    ),
    linetype = 'dashed',
    colour = 'red',
    alpha = 0.8
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  ggplot2::labs(
    x = NULL,
    y = 'b(tmax)'
  ) +
  ggplot2::ggtitle(
    label = 'b(tmax) vs tiempo'
  )

####################
# Rolling

nb_model_formula <- stats::as.formula(
  vivax ~ aet + prcp + soilm + tmax
)


## Get model coefficients
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

## Rolling NB regression

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
  
  return(coefficients)
}

rolling_nb_regression(
  data = region_dataset,
  formula = nb_model_formula,
  index = 'report_date',
  window = 12
) -> rolling_nb_coef_loreto

## Plotting
rolling_nb_coef_loreto %>% 
  tidyr::pivot_longer(
    cols = c(
      intercept,
      aet,
      prcp,
      soilm,
      tmax
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
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
    data = control_dates,
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

rolling_nb_regression(
  data = dataset,
  formula = nb_model_formula,
  index = 'report_date',
  window = 12
) -> rolling_nb_all_dataset

rolling_nb_all_dataset %>% 
  tidyr::pivot_longer(
    cols = c(
      intercept,
      aet,
      prcp,
      soilm,
      tmax
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
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
    data = control_dates,
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

#### Rolling correlation

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

rolling_correlations <- function(data, cols, index, window) {
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

rolling_correlations(
  data = region_dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'soilm',
    'tmax'
  ),
  index = 'report_date',
  window = 12
) -> rolling_cor_loreto

rolling_cor_loreto %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      soilm,
      tmax
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
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
    data = control_dates,
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

# Static correlations

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
      names_to = 'variable'
    )
  
  return(df_correlations_pivot)
}

get_static_correlations(
  data = region_dataset,
  cols = c(
    'vivax',
    'aet',
    'prcp',
    'soilm',
    'tmax'
  )
) -> static_cor_region

static_cor_region

rolling_cor_loreto %>% 
  tidyr::pivot_longer(
    cols = c(
      aet,
      prcp,
      soilm,
      tmax
    ),
    names_to = 'variable',
    values_to = 'value'
  ) %>% 
  ggplot2::ggplot(
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
    data = static_cor_region,
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
    ncol = 1,
    facets = . ~ variable,
    scales = 'free'
  ) +
  ggplot2::labs(
    y = NULL, 
    x = NULL
  )


