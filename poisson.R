View(dt_final)
summary(dt_final)

library(tidyverse)
library(magrittr)
library(pscl)
library(MASS)
library(slider)


# Convertir el año en fecha
lubridate::parse_date_time(dt_final$year, "Y")
lubridate::year(lubridate::parse_date_time(dt_final$year, "Y"))
dt_final$year <- lubridate::year(lubridate::parse_date_time(dt_final$year, "Y"))

# Convertir el mes en fecha
lubridate::parse_date_time(dt_final$month, "m")
lubridate::month(lubridate::parse_date_time(dt_final$month, "m"))
dt_final$month <- lubridate::month(lubridate::parse_date_time(dt_final$month, "m"))

# Grafica exploratoria
library(corrr)
dt_final %>% 
  select(falciparum:tmax) %>% 
  correlate() %>% 
  rplot(shape = 15, colors = c("red", "green"))


dt_final %>% 
  select(NOMBDIST, year, month, falciparum) %>% 
  mutate(fecha = lubridate::make_date(year = year, month = month, day = 1L)) %>%
  ggplot(aes(x=fecha, y=falciparum, group = NOMBDIST, colour = NOMBDIST)) +
    theme(legend.position = "none") +
    geom_line()+facet_wrap(.~NOMBDIST)


# Histograma de pop2015
ggplot(dt_final)+
  geom_histogram(aes(pop2015))

# Agrupar por Distritos
distritos_df <- dt_final %>%
  mutate_if(is.factor,as.character) %>%
  select(NOMBDIST) %>%
  unique()

for(i in distritos){
  sb = subset(x = dt_final, subset = NOMBDIST == i)
  assign(gsub(" ", "", (paste0("df_",i))), sb)
}


# Modelo Poisson 1
p1 <- glm(falciparum ~ aet + prcp + q + soilm + tmax + tmin, data = dt_final, family = poisson(link = "log"))
summary(p1)
logLik(p1)
exp(coef(p1))

E2 <- resid(p1, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(p1))   
sum(E2^2) / (N - p)

# Modelo Binomial Negativo 1
nb1 <- glm.nb(falciparum ~ aet + prcp + q + soilm + tmax + tmin, data = dt_final)
summary(nb1)
logLik(nb1)

E2 <- resid(nb1, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p)

# Porcentaje de 0
100*sum(dt_final$falciparum == 0)/nrow(dt_final)

# Modelo Poisson con inflacion zero 1
zp1 <- zeroinfl(falciparum ~ aet + prcp + q + soilm + tmax + tmin, dist = 'poisson', data = dt_final)
summary(zp1)

E2 <- resid(zp1, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(zp1))  
sum(E2^2) / (N - p)

# Modelo Binomial Negativo con inflacion zero 1
znb1 <- zeroinfl(falciparum ~ aet + prcp + q + soilm + tmax + tmin, dist = 'negbin', data = dt_final)
summary(znb1)
logLik(znb1)

E2 <- resid(znb1, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(znb1))
sum(E2^2) / (N - p)

# Modelo Binomial Negativo con inflacion zero y offset 2
znb2 <- zeroinfl(falciparum ~ aet + prcp + q + soilm + tmax + tmin, offset = log(pop2015), dist = 'negbin', data = dt_final)
summary(znb2)

E2 <- resid(znb2, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(znb2))
sum(E2^2) / (N - p)

# Modelo Binomial Negativo con inflacion zero 3 -(prcp,q)
znb3 <- zeroinfl(falciparum ~ aet + soilm + tmax + tmin, dist = 'negbin', data = dt_final)
summary(znb3)

E2 <- resid(znb3, type = "pearson")
N  <- nrow(dt_final)
p  <- length(coef(znb3))
sum(E2^2) / (N - p)

vuong(znb3, znb1)

AIC(znb1)
AIC(znb2)
AIC(znb3)

dt_final %<>% 
  add_column(fecha = lubridate::make_date(year = dt_final$year, month = dt_final$month, day = 1L))

dt_final %<>%
  arrange(fecha)

View(dt_final)

df_3 <- slide_period_dfr(
  .x = dt_final,
  .i = dt_final$fecha,
  .period = "month",
  ~data.frame(
    intercepto = coef(lm(falciparum ~ aet,data = .x))[[1]],
    x1 = coef(lm(falciparum ~ aet,data = .x))[[2]]
  ),
  .after = 6,
  .every = 1
)
View(df_3)


ggplot(data = df_3, aes(x = as.numeric(row.names(df_3)), y=x1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()

df_4 <- slide_period_dfr(
  .x = dt_final,
  .i = dt_final$fecha,
  .period = "month",
  ~data.frame(
    intercepto = coef(zeroinfl(falciparum ~ aet+prcp, dist = "negbin", data = .x))[[1]],
    x1 = coef(zeroinfl(falciparum ~ aet+prcp, dist = "negbin", data = .x))[[2]],
    x2 = coef(zeroinfl(falciparum ~ aet+prcp, dist = "negbin", data = .x))[[3]]
  ),
  .after = 12,
  .complete = TRUE
)
View(df_4)


ggplot(data = df_4, aes(x = as.numeric(row.names(df_4)), y=x1)) +
  labs(y= "Beta (aet)", x = "Año") +
  geom_line()
