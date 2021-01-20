#- Data loading 

data_path <- file.path('_data/') # Data directory path
file_name <- 'dt_final.csv' # csv. file name 
file_path <- file.path(data_path, file_name) # File path

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
  id_district = col_character()
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

raw_dataset %>% 
  dplyr::mutate(
    reporting_date = 
      lubridate::make_datetime(
        year = year,
        month = month,
        day = 1L 
      ) 
  ) %>% 
  dplyr::mutate(
    intervention = 
      ifelse(
        test = reporting_date < '2005-09-01',
        yes = 1,
        no = ifelse(
          test = reporting_date < '2010-08-01',
          yes = 2,
          no = 3
        )
      ),
    time = as.numeric(
      factor(
        x = reporting_date, 
        labels = c(as.character(seq(1, 216)))
      )
    )
  ) %>% 
  dplyr::arrange(reporting_date) -> 
  dataset # Final data frame

summary(dataset)
length(unique(dataset$district))
length(unique(dataset$province))

#- EDA 
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
  dplyr::filter(
    falciparum > 0,
    vivax > 0
  ) %>% 
  dplyr::select(
    aet,
    prcp,
    q,
    soilm,
    tmax,
    tmin,
    time,
    falciparum,
    vivax
  ) %>% 
  GGally::ggpairs(
    progress = FALSE
  )

dataset %>%
  dplyr::filter(
    falciparum > 0,
    vivax > 0
  ) %>%
  dplyr::select(
    aet,
    prcp,
    q,
    soilm,
    tmax,
    tmin,
    time,
    falciparum,
    vivax
  ) %>% 
  stats::cor()

dataset %>% 
  dplyr::mutate(
    district = as.numeric(
      factor(
        x = district, 
        labels = c(as.character(seq(1, 49)))
      )
    ),
    province = as.numeric(
      factor(
        x = province, 
        labels = c(as.character(seq(1, 8)))
      )
    )
  ) %>% 
  dplyr::select(
    reporting_date,
    falciparum,
    vivax,
    aet,
    prcp,
    q,
    soilm,
    tmax,
    tmin,
    time,
    district,
    province,
    year,
    month,
    pop2015
  ) ->
  model_data

cont_variables <- 
  c(
    "aet",
    "prcp",
    "q",
    "soilm",
    "tmax",
    "tmin"
  )

mc_cont_variables <- 
  c(
    "aet_mc",
    "prcp_mc",
    "q_mc",
    "soilm_mc",
    "tmax_mc",
    "tmin_mc"
  )

model_data[, mc_cont_variables] <- scale(
  x = model_data[, cont_variables], 
  scale = FALSE
)

std_cont_variables <- 
  c(
    "aet_std",
    "prcp_std",
    "q_std",
    "soilm_std",
    "tmax_std",
    "tmin_std"
  )

model_data[, std_cont_variables] <- scale(
  x = model_data[, cont_variables], 
  scale = TRUE
)

#- GLM model

vivax_glm_full <- MASS::glm.nb(
  formula = vivax ~ aet + prcp + q + soilm + tmax + tmin, 
  data = model_data
)

summary(vivax_glm_full)

library(lme4)

vivax_glmer_nb_full <- glmer.nb(
  formula = vivax ~ aet + prcp + q + soilm + tmax + tmin + (1|district),
  data = model_data,
  control = glmerControl(
    optimizer = "bobyqa"
  )
)

# Does not converge for the second module
# tmax and tmin significant 

#########################

vivax_glmer_nb_null <- glmer.nb(
  formula = vivax ~ offset(log(pop2015)) + (1|year/month) + (1|province/district),
  data = model_data
)

summary(vivax_glmer_nb_null)
AIC(vivax_glmer_nb_null)

vivax_glmer_nb_aet <- update(
  vivax_glmer_nb_null,
  vivax ~ . + aet
)

# Unidentifiable

vivax_glmer_nb_aet <- update(
  vivax_glmer_nb_null,
  vivax ~ . + aet_mc
)

# Unidentifiable

vivax_glmer_nb_aet <- update(
  vivax_glmer_nb_null,
  vivax ~ . + aet_std
)

summary(vivax_glmer_nb_aet)
AIC(vivax_glmer_nb_null, vivax_glmer_nb_aet)

vivax_glmer_nb_prcp <- update(
  vivax_glmer_nb_null,
  vivax ~ . + prcp
)

# Failed to converge / Unidentifiable

vivax_glmer_nb_prcp <- update(
  vivax_glmer_nb_null,
  vivax ~ . + prcp_mc
)

# Failed to converge / Unidentifiable

vivax_glmer_nb_prcp <- update(
  vivax_glmer_nb_null,
  vivax ~ . + prcp_std
)

summary(vivax_glmer_nb_prcp)
AIC(
  vivax_glmer_nb_null,
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp
)

vivax_glmer_nb_q <- update(
  vivax_glmer_nb_null,
  vivax ~ . + q
)

# Failed to converge / Unidentifiable

vivax_glmer_nb_q <- update(
  vivax_glmer_nb_null,
  vivax ~ . + q_mc
)

# Failed to converge / Unidentifiable

vivax_glmer_nb_q <- update(
  vivax_glmer_nb_null,
  vivax ~ . + q_std
)

summary(vivax_glmer_nb_q)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q
)

vivax_glmer_nb_soilm <- update(
  vivax_glmer_nb_null,
  vivax ~ . + soilm
)

# Unidentifiable

vivax_glmer_nb_soilm <- update(
  vivax_glmer_nb_null,
  vivax ~ . + soilm_mc
)

# Unidentifiable

vivax_glmer_nb_soilm <- update(
  vivax_glmer_nb_null,
  vivax ~ . + soilm_std
)

summary(vivax_glmer_nb_soilm)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm
)

vivax_glmer_nb_tmax <- update(
  vivax_glmer_nb_null,
  vivax ~ . + tmax
)

vivax_glmer_nb_tmax <- update(
  vivax_glmer_nb_null,
  vivax ~ . + tmax_std
)

summary(vivax_glmer_nb_tmax)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax
)

vivax_glmer_nb_tmin <- update(
  vivax_glmer_nb_null,
  vivax ~ . + tmin
)

vivax_glmer_nb_tmin <- update(
  vivax_glmer_nb_null,
  vivax ~ . + tmin_std
)

summary(vivax_glmer_nb_tmin)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin
)

vivax_glmer_nb_21 <- update(
  vivax_glmer_nb_prcp,
  vivax ~ . + aet_std
)

summary(vivax_glmer_nb_21)
anova(vivax_glmer_nb_null, vivax_glmer_nb_21)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21
)

vivax_glmer_nb_23 <- update(
  vivax_glmer_nb_prcp,
  vivax ~ . + q_std
)

summary(vivax_glmer_nb_23)
anova(vivax_glmer_nb_null, vivax_glmer_nb_23)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23
)

vivax_glmer_nb_24 <- update(
  vivax_glmer_nb_prcp,
  vivax ~ . + soilm_std
)

summary(vivax_glmer_nb_24)
anova(vivax_glmer_nb_null, vivax_glmer_nb_24)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24
)

vivax_glmer_nb_25 <- update(
  vivax_glmer_nb_prcp,
  vivax ~ . + tmax_std
)

summary(vivax_glmer_nb_25)
anova(vivax_glmer_nb_null, vivax_glmer_nb_25)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25
)

vivax_glmer_nb_26 <- update(
  vivax_glmer_nb_prcp,
  vivax ~ . + tmin_std
)

summary(vivax_glmer_nb_26)
anova(vivax_glmer_nb_prcp, vivax_glmer_nb_26)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25,
  vivax_glmer_nb_26
)

vivax_glmer_nb_251 <- update(
  vivax_glmer_nb_25,
  vivax ~ . + aet_std
)

summary(vivax_glmer_nb_251)
anova(vivax_glmer_nb_null, vivax_glmer_nb_251)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25,
  vivax_glmer_nb_26,
  vivax_glmer_nb_251
)

vivax_glmer_nb_254 <- update(
  vivax_glmer_nb_25,
  vivax ~ . + soilm_std
)

summary(vivax_glmer_nb_254)
anova(vivax_glmer_nb_null, vivax_glmer_nb_254)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25,
  vivax_glmer_nb_26,
  vivax_glmer_nb_251,
  vivax_glmer_nb_254
)

vivax_glmer_nb_256 <- update(
  vivax_glmer_nb_25,
  vivax ~ . + tmin_std
)

summary(vivax_glmer_nb_256)
anova(vivax_glmer_nb_null, vivax_glmer_nb_256)
AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25,
  vivax_glmer_nb_26,
  vivax_glmer_nb_251,
  vivax_glmer_nb_254,
  vivax_glmer_nb_256
)

library(mgcv)

vivax_gam_full <- gam(
  formula = vivax ~ aet + prcp + q + soilm + tmax + tmin + s(district, bs="re"),
  data = model_data,
  family = nb(),
  method = "REML"
)

summary(vivax_gam_full)

AIC(vivax_glm_full, vivax_glmer_nb_v1, vivax_glmer_v1, vivax_gam_full)

library(gamm4)

vivax_gamm_nb_25 <- gamm4(
  vivax ~ 
    offset(log(pop2015)) + 
    s(time, by = prcp_std) + 
    s(time, by = tmax_std),
  random = ~ offset(log(pop2015)) + (1|year/month) + (1|province/district),
  family = negbin(theta=getME(vivax_glmer_nb_25, "glmer.nb.theta")),
  data = model_data,
  REML = TRUE
)

summary(vivax_gamm_nb_25$mer)
summary(vivax_gamm_nb_25$gam)

AIC(
  vivax_glmer_nb_null, 
  vivax_glmer_nb_aet,
  vivax_glmer_nb_prcp, 
  vivax_glmer_nb_q,
  vivax_glmer_nb_soilm,
  vivax_glmer_nb_tmax,
  vivax_glmer_nb_tmin,
  vivax_glmer_nb_21,
  vivax_glmer_nb_23,
  vivax_glmer_nb_24,
  vivax_glmer_nb_25,
  vivax_glmer_nb_26,
  vivax_glmer_nb_251,
  vivax_glmer_nb_254,
  vivax_glmer_nb_256,
  vivax_gamm_nb_25$mer
)

#### Check model assumptions

#### Rolling

library(slider)

vivax_rolling_glmer_nb_24 <- slider::slide_period_dfr(
  .x = model_data,
  .i = model_data$reporting_date,
  .period = 'month',
  .f = ~ glmer.nb(
    vivax ~ 
      offset(log(pop2015)) + 
      prcp_std +
      tmax_std +
      # (1|year/month) + 
      (1|province/district),
    data = .x
  ),
  .every = 1,
  .before = 23,
  .complete = TRUE
)





