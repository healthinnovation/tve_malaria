library(tidyverse)
library(corrr)
library(magrittr)
library(MASS)
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
  select(falciparum:tmax) %>% 
  correlate() %>% 
  rplot(shape = 15, colors = c("red", "green"))

# Incidencia  de falciparum en el tiempo por distrito
dt_final %>% 
  select(NOMBDIST, year, month, falciparum) %>% 
  mutate(fecha = lubridate::make_date(year = year, month = month, day = 1L)) %>%
  ggplot(aes(x=fecha, y=falciparum, group = NOMBDIST, colour = NOMBDIST)) +
    theme(legend.position = "none") +
    geom_line()+facet_wrap(.~NOMBDIST)

# Incidencia  de vivax en el tiempo por distrito
dt_final %>% 
  select(NOMBDIST, year, month, vivax) %>% 
  mutate(fecha = lubridate::make_date(year = year, month = month, day = 1L)) %>%
  ggplot(aes(x=fecha, y=vivax, group = NOMBDIST, colour = NOMBDIST)) +
  theme(legend.position = "none") +
  geom_line()+facet_wrap(.~NOMBDIST)

# Agrupar por Distritos
df<-dt_final %>%
  group_by(NOMBDIST) %>% 
  nest() %>% 
  mutate(model=map(data,myfunct))
  
# Modelo Poisson
p <- glm(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final, family = poisson(link = "log"))
summary(p)
logLik(p)

# Modelo Binomial Negativo
nb <- glm.nb(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final)
coefficients(glm.nb(falciparum ~ aet + prcp + q + soilm + tmax, data = dt_final))[[2]]
nb$coefficients
summary(nb)
logLik(nb)

# Rolling regression binomial negativa
myfunct<-function(d){
  df<-slide_period_dfr(
    .x = d,
    .i = d$fecha,
    .period = "month",
    ~data.frame(
      intercepto = coef(lm(falciparum ~ aet,data = .x))[[1]],
      beta1 = coef(lm(falciparum ~ aet,data = .x))[[2]]
    ),
    .every = 12
  )
} 

res<-myfunct(dt_final)
res


for(i in seq(1,49)){
  print(ggplot(data = df$model[[i]], aes(x = seq(2000,2017), y=beta1)) +
    labs(y= "Beta (aet)", x = "Año") +
    geom_line()
  )
}

ggplot(data = df$model[[1]], aes(x = seq(2000,2017), y=beta1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()



myfunct<-function(d){
  df<-slide_period_dfr(
    .x = d,
    .i = d$fecha,
    .period = "month",
    ~data.frame(
      intercepto = coef(glm.nb(vivax ~ aet + prcp + q + soilm + tmax, data = .x))[[1]],
      beta1 = coef(glm.nb(vivax ~ aet + prcp + q + soilm + tmax, data = .x))[[2]]
    ),
    .every = 12
  )
} 

res<-myfunct(dt_final)

ggplot(data = res, aes(x = seq(2000,2017), y=beta1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()