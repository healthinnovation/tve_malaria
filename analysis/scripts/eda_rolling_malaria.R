# 1 - Reading data
library(tidyverse)
theme_set(theme_light())

data_path <- file.path('_data/')
file_name <- 'dt_final.csv'
file_path <- file.path(data_path, file_name)

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

raw_dataset <- readr::read_csv(
  file = file_path,
  col_names = col_names,
  col_types = col_types,
  skip = 1
)

View(raw_dataset)

# Make date column
raw_dataset %>% 
  dplyr::mutate(
    date = lubridate::make_datetime(
      year = year,
      month = month,
      day = 1L,
    ),
  ) %>% 
  dplyr::arrange(date) -> 
  dataset

dataset
view(dataset)

# Make control column
dataset %>% 
  mutate(
    control = ifelse(
      test = (date >= '2005-09-01' & date <= '2010-09-01'),
      yes = 1,
      no = 0
    )
  ) ->
  dataset

# 2 - Data validation

summary(dataset)

dataset %>% 
  dplyr::select(date) %>% 
  dplyr::distinct() %>% 
  dplyr::count()

dataset %>% 
  dplyr::select(district) %>% 
  dplyr::distinct() %>% 
  dplyr::count()
  
dataset %>% 
  dplyr::group_by(date) %>% 
  dplyr::count() 

dataset %>% 
  dplyr::group_by(district) %>% 
  dplyr::count() %>% 
  View()

dataset %>% 
  dplyr::select(province) %>% 
  dplyr::distinct() %>% 
  dplyr::count()

# 2.1 - Falciparum

dataset %>% 
  dplyr::group_by(district) %>% 
  dplyr::summarise(
    n = n(),
    sum = sum(falciparum),
    min = min(falciparum),
    max = max(falciparum),
    no_cases = round(
      x = 100 * sum(falciparum < 1) / n(),
      digits = 2
    )
  ) %>% 
  View()

# 2.2 - Vivax

dataset %>% 
  dplyr::group_by(district) %>% 
  dplyr::summarise(
    n = n(),
    sum = sum(vivax),
    min = min(vivax),
    max = max(vivax),
    no_cases = round(
      x = 100 * sum(vivax < 1) / n(),
      digits = 2
    )
  ) %>% 
  View()

# 2.3 - Malaria (Falciparum + Vivax)
dataset %>% 
  mutate(
    malaria = falciparum + vivax
  ) -> dataset

dataset %>% 
  dplyr::group_by(district) %>% 
  dplyr::summarise(
    n = n(),
    sum = sum(malaria),
    min = min(malaria),
    max = max(malaria),
    no_cases = round(
      x = 100 * sum((malaria) < 1) / n(),
      digits = 2
    )
  ) %>% 
  View()

# 3 - Exploratory Data Analysis

# Reported cases in Loreto
dataset %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    reported_cases = sum(malaria)
  ) %>% 
  ggplot(aes(x = date, y = reported_cases)) +
  geom_area(
    colour = '#E16724',
    fill = '#E16724',
    alpha = 0.25
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  )

# Reported cases by district
dataset %>% 
  dplyr::group_by(district, date) %>% 
  dplyr::summarise(
    reported_cases = sum(malaria)
  ) %>% 
  ggplot(aes(x = date, y = reported_cases)) +
  geom_area(
    colour = '#E16724',
    fill = '#E16724',
    alpha = 0.25
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '5 year'
  ) +
  labs(
    y = 'Number of reported cases (x1000)', 
    x = NULL
  ) +
  facet_wrap(
    facets = .~district,
    scales = 'free'
  )

# Reported cases by province
dataset %>% 
  dplyr::group_by(province, date) %>% 
  dplyr::summarise(
    reported_cases = sum(malaria)
  ) %>% 
  ggplot(aes(x = date, y = reported_cases)) +
  geom_area(
    colour = '#E16724',
    fill = '#E16724',
    alpha = 0.25
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '5 year'
  ) +
  labs(
    y = 'Number of reported cases (x1000)', 
    x = NULL
  ) +
  facet_wrap(
    facets = .~province,
    scales = 'free'
  )

# Reported cases by bacteria
dataset %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    falciparum = sum(falciparum),
    vivax = sum(vivax)
  ) %>% 
  pivot_longer(
    cols = c(falciparum, vivax),
    names_to = 'parasite'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_area(
    aes(
      colour = parasite,
      fill = parasite
    ),
    alpha = 0.25,
    size = 1,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_fill_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  ) 

# Reported cases by bacteria and district

dataset %>% 
  dplyr::group_by(district, date) %>% 
  dplyr::summarise(
    falciparum = sum(falciparum),
    vivax = sum(vivax)
  ) %>% 
  pivot_longer(
    cols = c(falciparum, vivax),
    names_to = 'parasite'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_area(
    aes(
      colour = parasite,
      fill = parasite
    ),
    alpha = 0.25,
    size = 1,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_fill_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '5 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  ) +
  facet_wrap(
    facets = .~district,
    scales = 'free'
  )

# Reported cases by bacteria and province

dataset %>% 
  dplyr::group_by(province, date) %>% 
  dplyr::summarise(
    falciparum = sum(falciparum),
    vivax = sum(vivax)
  ) %>% 
  pivot_longer(
    cols = c(falciparum, vivax),
    names_to = 'parasite'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_area(
    aes(
      colour = parasite,
      fill = parasite
    ),
    alpha = 0.25,
    size = 1,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_fill_manual(
    values = c("#00AFBB", "#E7B800")
  ) +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '5 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  ) +
  facet_wrap(
    facets = .~province,
    scales = 'free'
  )

# Seasonal plot - Number of reported cases
library(forecast)

dataset %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    reported_cases = sum(malaria) 
  ) %>% 
  dplyr::select(reported_cases) %>% 
  ts(
    start = 2000,
    frequency = 12
  ) %>% 
  forecast::ggseasonplot(
    polar = TRUE
    ) +
  ggtitle("Seasonal plot: Number of reported cases")
  
dataset %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    reported_cases = sum(malaria) 
  ) %>% 
  dplyr::select(reported_cases) %>% 
  ts(
    start = 2000,
    frequency = 12
  ) %>% 
  forecast::ggsubseriesplot(
    polar = TRUE
  ) +
  ggtitle("Seasonal plot: Number of reported cases")

# Reported cases vs. max temperature

# All districts
dataset %>%
  ggplot(aes(x = tmax, y = malaria)) +
  geom_point() +
  geom_smooth(
    method = 'loess'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = 'Max Temperature'
  ) +
  facet_wrap(
    facets = . ~ district,
    scales = 'free'
  )

# Las Amazonas district
dataset %>% 
  filter(district == 'LAS AMAZONAS') %>% 
  ggplot(aes(x = date, y = malaria)) +
  geom_line() +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ggAcf(
    lag.max = 24
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  diff(
    differences = 1
  ) %>% 
  ggAcf()

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  Box.test(
    type = 'Ljung-Box'
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>%
  ndiffs()

library(GGally)

dataset %>% 
  dplyr::filter(district == 'LAS AMAZONAS') %>%  
  select(
    aet,
    prcp,
    soilm,
    tmax,
    tmin,
    incidence
  ) %>% 
  ggpairs()

dataset %>% 
  dplyr::filter(district == 'LAS AMAZONAS') %>% 
  ggplot(
    aes(
      x = tmax, 
      y = malaria
    )
  ) +
  geom_point() +
  geom_smooth(
    method = 'loess'
  ) +
  labs(
    y = 'Incidence rate (x1000)', 
    x = 'Max Temperature'
  )

dataset %>% 
  dplyr::filter(district == 'LAS AMAZONAS') %>%  
  select(
    date,
    malaria,
    tmax
  ) %>% 
  pivot_longer(
    cols = c(malaria, tmax),
    names_to = 'variable'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  facet_grid(
    facets = variable ~ .,
    scales = 'free'
  )

# Distribution fitting
dataset %>% 
  filter(
    district == 'LAS AMAZONAS'
  ) %>% 
  ggplot(aes(x = malaria)) +
  geom_histogram(
    bins = 30
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS'
  ) %>% 
  group_by(malaria) %>% 
  summarise(
    freq = n()
  ) %>% 
  mutate(
    percent = 100 * (freq / sum(freq))
  ) %>% View()

dataset %>% 
  filter(
    district == 'LAS AMAZONAS'
  ) %>% 
  summarise(
    mean = mean(malaria),
    sd = sd(malaria),
    var = sd(malaria)^2
  )

# Poisson
summary(
  glm(
    formula = malaria ~ 1,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'poisson'
  )
)

logLik(
  glm(
    formula = malaria ~ 1,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'poisson'
  )
)

summary(
  glm(
    formula = malaria ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'poisson'
  )
)

logLik(
  glm(
    formula = malaria ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'poisson'
  )
)

# Negative Binomial
library(MASS)

summary(
  glm.nb(
    formula = malaria ~ 1,
    data = dataset,
    subset = (district == 'LAS AMAZONAS')
  )
)

logLik(
  glm.nb(
    formula = malaria ~ 1,
    data = dataset,
    subset = (district == 'LAS AMAZONAS')
  )
)

summary(
  glm.nb(
    formula = malaria ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS')
  )
)

logLik(
  glm.nb(
    formula = malaria ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS')
  )
)

# Binomial
summary(
  glm(
    formula = (malaria > 0) ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'binomial'
  )
)

logLik(
  glm(
    formula = (malaria > 0) ~ tmax,
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    family = 'binomial'
  )
)

# ZT Negative Binomial
library(VGAM)

summary(
  vglm(
    formula = malaria ~ tmax, 
    data = dataset,
    subset = (district == 'LAS AMAZONAS' & malaria != 0),
    family = posnegbinomial
  )
)

logLik(
  vglm(
    formula = malaria ~ tmax, 
    data = dataset,
    subset = (district == 'LAS AMAZONAS' & malaria != 0),
    family = posnegbinomial
  )
)

# Hurdle
library(pscl)

summary(
  hurdle(
    formula = malaria ~ tmax, 
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    dist = 'negbin',
    zero.dist = 'binomial',
    separate = TRUE
  )
)

logLik(
  hurdle(
    formula = malaria ~ tmax, 
    data = dataset,
    subset = (district == 'LAS AMAZONAS'),
    dist = 'negbin',
    zero.dist = 'binomial'
  )
)

######################################
# Analysis - Iquitos
dataset %>% 
  filter(
    district == 'IQUITOS'
  ) %>% 
  ggplot(
    aes(x = malaria)
  ) +
  geom_histogram(
    bins = 50,
    color = 'black',
    fill = 'white'
  )

dataset %>% 
  filter(
    district == 'IQUITOS'
  ) %>% 
  group_by(malaria) %>% 
  summarise(
    freq = n()
  ) %>% 
  mutate(
    percent = 100 * (freq / sum(freq)),
    cumulative = cumsum(100 * (freq / sum(freq)))
  ) %>% View()

dataset %>% 
  filter(
    district == 'IQUITOS'
  ) %>% 
  select(
    date,
    malaria,
    aet,
    prcp,
    soilm,
    tmax,
    control
  ) %>% 
  pivot_longer(
    cols = c(
      malaria, 
      aet,
      prcp,
      soilm,
      tmax,
      control
    ),
    names_to = 'variable'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  facet_grid(
    facets = variable ~ .,
    scales = 'free'
  )
 
dataset %>% 
  filter(
    district == 'IQUITOS'
  ) %>% 
  group_by(
    control
  ) %>% 
  summarise(
    mean = mean(malaria)
  ) %>% 
  ggplot(
    aes(
      x = as.factor(control),
      y = mean,
      fill = as.factor(control)
    )
  ) +
  geom_col() +
  labs(
    x = 'control',
    fill = 'control'
  )

dataset %>% 
  filter(
    district == 'IQUITOS'
  ) %>% 
  ggplot(
    aes(
      x = as.factor(control),
      y = malaria,
      fill = as.factor(control)
    )
  ) +
  geom_boxplot() +
  labs(
    x = 'control',
    fill = 'control'
  ) 

dataset %>% 
  dplyr::filter(district == 'LAS AMAZONAS') %>%  
  select(
    aet,
    prcp,
    soilm,
    tmax,
    control,
    malaria
  ) %>% 
  ggpairs()

dataset %>% 
  filter(district == 'IQUITOS') %>% 
  ggplot(aes(x = date, y = malaria)) +
  geom_line() +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  labs(
    y = 'Number of reported cases', 
    x = NULL
  )

dataset %>% 
  filter(
    district == 'IQUITOS',
    malaria > 0
  ) %>% 
  ggplot(aes(x = date, y = log(malaria))) +
  geom_line() +
  scale_x_datetime(
    date_labels = '%Y',
    date_breaks = '1 year'
  ) +
  labs(
    y = 'Number of reported cases (log scale)', 
    x = NULL
  )

dataset %>% 
  filter(
    district == 'IQUITOS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  ggAcf(
    lag.max = 24
  )

dataset %>% 
  filter(
    district == 'IQUITOS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  log() %>% 
  ggAcf(
    lag.max = 24
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  diff(
    differences = 1,
    lag = 2
  ) %>% 
  ggAcf(
    lag.max = 24
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  Box.test(
    type = 'Ljung-Box'
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>% 
  diff(
    differences = 1
  ) %>%
  Box.test(
    type = 'Ljung-Box'
  )

dataset %>% 
  filter(
    district == 'LAS AMAZONAS',
    malaria > 0
  ) %>%
  select(
    malaria
  ) %>% 
  ts() %>%
  ndiffs()

# Tmax

