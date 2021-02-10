library(readr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lme4, warn.conflicts = FALSE)
library(slider, warn.conflicts = FALSE)
library(gamm4, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tictoc)

## Rolling analysis

ggplot2::theme_set(theme_light())

df_intervention_period <- 
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



vivax_rolling_glmer <- function(data) {
  model <- update(
    vivax_glm_nb_base,
    vivax ~ . + prcp_std + tmax_std,
    data    = data
  )
  
  fit <- lme4::fixef(model)[2:3]
  fit_se <- sqrt(diag(vcov(model)))[2:3]
  names(fit_se) <- names(fit)
  
  df <- tibble::tibble(
    reporting_date = max(data$reporting_date),
    predictor      = names(fit),
    fit            = fit,
    fit_se         = fit_se
  )
  return(df)
}


### 6 month window

tictoc::tic("Rolling regression")
vivax_rolling_glm_nb_6 <- slider::slide_period_dfr(
  .x        = dataset,
  .i        = dataset$reporting_date,
  .period   = 'month',
  .f        = vivax_rolling_glmer,
  .every    = 1,
  .before   = 5,
  .complete = TRUE
)
tictoc::toc()

vivax_rolling_glm_nb_6 %>% 
  ggplot2::ggplot(
    aes(x = reporting_date, y = fit)
  ) + 
  ggplot2::geom_line(aes(y = fit)) +
  ggplot2::geom_ribbon(
    aes(
      ymin = fit - 2 * fit_se,
      ymax = fit + 2 * fit_se
    ),
    fill  = "grey70",
    alpha = 0.8
  ) +
  ggplot2::scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  ggplot2::geom_vline(
    aes(xintercept = as.numeric(dates)),
    data     = df_intervention_period,
    linetype = "dashed",
    colour   = "red"
  ) +
  ggplot2::geom_hline(
    aes(yintercept = 0),
    linetype = 'dashed',
    alpha    = 0.6
  ) +
  ggplot2::labs(
    y = NULL, 
    x = NULL
  ) +
  ggplot2::facet_wrap(
    ncol   = 1,
    facets = . ~ predictor
  )

### 12 month window


tictoc::tic("Rolling regression")
vivax_rolling_glm_nb_12 <- slider::slide_period_dfr(
  .x        = dataset,
  .i        = dataset$reporting_date,
  .period   = 'month',
  .f        = vivax_rolling_glmer,
  .every    = 1,
  .before   = 11,
  .complete = TRUE
)
tictoc::toc()


vivax_rolling_glm_nb_12 %>% 
  ggplot2::ggplot(
    aes(x = reporting_date, y = fit)
  ) + 
  ggplot2::geom_line(aes(y = fit)) +
  ggplot2::geom_ribbon(
    aes(
      ymin = fit - 2 * fit_se,
      ymax = fit + 2 * fit_se
    ),
    fill  = "grey70",
    alpha = 0.8
  ) +
  ggplot2::scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  ggplot2::geom_vline(
    aes(xintercept = as.numeric(dates)),
    data     = df_intervention_period,
    linetype = "dashed",
    colour   = "red"
  ) +
  ggplot2::geom_hline(
    aes(yintercept = 0),
    linetype = 'dashed',
    alpha    = 0.6
  ) +
  ggplot2::labs(
    y = NULL, 
    x = NULL
  ) +
  ggplot2::facet_wrap(
    ncol   = 1,
    facets = . ~ predictor
  )
