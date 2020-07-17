library(tidyverse)
library(corrr)
library(magrittr)
library(MASS)
library(pscl)
library(slider)

# Carga
dt_final<-read.csv("./_data/dt_final.csv")
head(dt_final)

# Configurar data
dt_final %<>% 
  mutate(
    year = lubridate::year(lubridate::parse_date_time(dt_final$year, "Y")),
    month = lubridate::month(lubridate::parse_date_time(dt_final$month, "m"))
  ) %>% 
  add_column(fecha = lubridate::make_date(year = dt_final$year, month = dt_final$month, day = 1L)) %>% 
  arrange(fecha)

# Matriz grafica de correlaciones
dt_final %>% 
  dplyr::select(falciparum:tmin) %>% 
  correlate() %>% 
  rplot(shape = 15, colors = c("red", "green"))

# Incidencia  de falciparum en el tiempo por distrito
dt_final %>% 
  dplyr::select(NOMBDIST, year, month, falciparum) %>% 
  mutate(fecha = lubridate::make_date(year = year, month = month, day = 1L)) %>%
  ggplot(aes(x=fecha, y=falciparum, group = NOMBDIST, colour = NOMBDIST)) +
    theme(legend.position = "none") +
    geom_line()+facet_wrap(.~NOMBDIST)

# Incidencia  de vivax en el tiempo por distrito
dt_final %>% 
  dplyr::select(NOMBDIST, year, month, vivax) %>% 
  mutate(fecha = lubridate::make_date(year = year, month = month, day = 1L)) %>%
  ggplot(aes(x=fecha, y=vivax, group = NOMBDIST, colour = NOMBDIST)) +
  theme(legend.position = "none") +
  geom_line()+facet_wrap(.~NOMBDIST)




  
# Modelo Poisson
p <- glm(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final, family = poisson(link = "log"))
summary(p)
logLik(p)

# Modelo Binomial Negativo
nb <- glm.nb(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final)
coef(nb)[[2]]
nb$coefficients
summary(nb)
logLik(nb)

# Modelo Hurdle
h <- hurdle(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final, dist = "negbin")
summary(h)
logLik(h)

# Rolling regression binomial negativa
df_betas <- slide_period_dfr(
  .x = dt_final,
  .i = dt_final$fecha,
  .period = "month",
  ~data.frame(
    Aet = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[2]],
    Prcp = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[3]],
    Solim = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[4]],
    Tmax = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[5]]
  ),
  .every = 1,
  .after = 11,
  .complete = T
)

df_betas

colnames(df_betas)

for (i in colnames(df_betas)) {
  print(i)
}

for (i in colnames(df_betas)) {
  print(ggplot(data = df_betas, aes(x = seq(1,205), y=.data[[i]])) +
          labs(y= "Coeficiente", x = "Mes") +
          labs(title=i) +
          geom_line())
}




###############################

# Agrupar por Distritos
df<-dt_final %>%
  group_by(NOMBDIST) %>% 
  nest() %>% 
  mutate(model=map(data,myfunct))

myfunct<-function(d){
  slide_period_dfr(
    .x = d,
    .i = d$fecha,
    .period = "month",
    ~data.frame(
      beta1 = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[2]],
      beta2 = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[3]],
      beta3 = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[4]],
      beta4 = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[5]]
    ),
    .every = 12,
    .complete = T
  )
}

res<-myfunct(dt_final)

ggplot(data = res, aes(x = seq(2000,2017), y=beta1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()

for(i in seq(1,49)){
  print(ggplot(data = df$model[[i]], aes(x = seq(2000,2017), y=beta1)) +
          labs(y= "Beta (aet)", x = "Año") +
          geom_line()
  )
}



ggplot(data = df$model[[1]], aes(x = seq(2000,2017), y=beta1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()