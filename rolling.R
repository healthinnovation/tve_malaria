library(tidyverse)
library(corrr)
library(magrittr)
library(MASS)
library(pscl)
library(slider)
library(purrr)
library(ggpubr)  

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

# Incidencia  de falciparum y vivax en el tiempo
dt_final %>% 
  group_by(fecha) %>% 
  summarise(
    sum_falciparum = sum(falciparum),
    sum_vivax = sum(vivax)
  ) %>% 
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = sum_falciparum, colour = "sum_falciparum")) +
  geom_line(aes(y = sum_vivax, colour = "sum_vivax")) +
  labs(y= "Incidencia", x = "Meses")

# Incidencia  de Falciparum y Vivax en el tiempo por provincia
dt_final %>% 
  group_by(NOMBPROV, fecha) %>% 
  summarise(
    sum_falciparum = sum(falciparum),
    sum_vivax = sum(vivax)
  ) %>% 
  ggplot(aes(x=fecha, group = NOMBPROV, colour = NOMBPROV)) +
  geom_line(aes(y=sum_falciparum, colour = "sum_falciparum")) +
  geom_line(aes(y=sum_vivax, colour = "sum_vivax")) +
  facet_wrap(.~NOMBPROV, scales = "free")

# Incidencia  de Falciparum y Vivax en el tiempo por distrito
dt_final %>% 
  dplyr::select(NOMBDIST, fecha, falciparum, vivax) %>% 
  ggplot(aes(x=fecha, group = NOMBDIST)) +
  geom_line(aes(y = falciparum, colour = "falciparum")) +
  geom_line(aes(y = vivax, colour = "vivax")) +
  facet_wrap(.~NOMBDIST, scales = "free")

# Modelo Poisson
p <- glm(falciparum ~ aet + prcp + soilm + tmax, data = dt_final, family = poisson(link = "log"))
summary(p)
logLik(p)

# Modelo Binomial Negativo
nb <- glm.nb(falciparum ~ aet + prcp + soilm + tmax, data = dt_final)
coef(nb)[[2]]
nb$coefficients
logLik(nb)
ci <- confint(nb)
ci[2,1]
ci[2,2]
# Modelo Hurdle
h <- hurdle(falciparum ~ aet + prcp + soilm + tmax, data = dt_final, dist = "negbin")
summary(h)
coef(h)
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
  .before = 6,
  .after = 5,
  .complete = T
)

df_betas <- slide_period_dfr(
  .x = dt_final,
  .i = dt_final$fecha,
  .period = "month",
  ~data.frame(
    Aet_falci = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[2]],
    Aet_falci_min = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[2,1],
    Aet_falci_max = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[2,2],
    Aet_vivax = coef(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[[2]],
    Aet_vivax_min = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[2,1],
    Aet_vivax_max = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[2,2],
    Aet_mean = mean(.x$aet),
    Prcp_falci = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[3]],
    Prcp_falci_min = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[3,1],
    Prcp_falci_max = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[3,2],
    Prcp_vivax = coef(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[[3]],
    Prcp_vivax_min = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[3,1],
    Prcp_vivax_max = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[3,2],
    Prcp_mean = mean(.x$prcp),
    Soilm_falci = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[4]],
    Soilm_falci_min = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[4,1],
    Soilm_falci_max = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[4,2],
    Soilm_vivax = coef(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[[4]],
    Soilm_vivax_min = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[4,1],
    Soilm_vivax_max = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[4,2],
    Soilm_mean = mean(.x$soilm),
    Tmax_falci = coef(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[[5]],
    Tmax_falci_min = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[5,1],
    Tmax_falci_max = confint(glm.nb(formula = falciparum ~ aet + prcp + soilm + tmax, data = .x))[5,2],
    Tmax_vivax = coef(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[[5]],
    Tmax_vivax_min = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[5,1],
    Tmax_vivax_max = confint(glm.nb(formula = vivax ~ aet + prcp + soilm + tmax, data = .x))[5,2],
    Tmax_mean = mean(.x$tmax)
    ),
  .every = 1,
  .before = 6,
  .after = 5,
  .complete = T
) 


df_betas2 <- df_betas%>% 
  gather(coeficientes, valor, Aet_falci:Tmax_mean) %>% 
  add_column(
    categoria = rep(c(rep("falci_coef",205),rep("falci_coef_min",205),rep("falci_coef_max",205),rep("vivax_coef",205),rep("vivax_coef_min",205),rep("vivax_coef_max",205),rep("promedio",205)),4),
    fecha = rep(seq(as.Date("2000/1/1"), by = "month", length.out = 205),28),
    variable = c(rep("Aet",1435),rep("Prcp",1435),rep("Soilm",1435),rep("Tmax",1435))
)

p1 <- ggplot(data = df_betas2, mapping =  aes(x = fecha)) +
  geom_ribbon(data = filter(df_betas2, categoria=="falci_coef_min" & variable=="Aet"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="falci_coef_min" & variable=="Aet"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="falci_coef_max" & variable=="Aet"),valor)), 
              alpha = 0.1, fill = "red", linetype = "dashed") +
  geom_ribbon(data = filter(df_betas2, categoria=="falci_coef_min" & variable=="Prcp"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="falci_coef_min" & variable=="Prcp"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="falci_coef_max" & variable=="Prcp"),valor)), 
              alpha = 0.1, fill = "red", linetype = "dashed") +
  geom_ribbon(data = filter(df_betas2, categoria=="falci_coef_min" & variable=="Soilm"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="falci_coef_min" & variable=="Soilm"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="falci_coef_max" & variable=="Soilm"),valor)), 
              alpha = 0.1, fill = "red", linetype = "dashed") +
  geom_ribbon(data = filter(df_betas2, categoria=="falci_coef_min" & variable=="Tmax"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="falci_coef_min" & variable=="Tmax"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="falci_coef_max" & variable=="Tmax"),valor)), 
              alpha = 0.1, fill = "red", linetype = "dashed") +
  geom_ribbon(data = filter(df_betas2, categoria=="vivax_coef_min" & variable=="Aet"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="vivax_coef_min" & variable=="Aet"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="vivax_coef_max" & variable=="Aet"),valor)), 
              alpha = 0.1, fill = "blue") +
  geom_ribbon(data = filter(df_betas2, categoria=="vivax_coef_min" & variable=="Prcp"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="vivax_coef_min" & variable=="Prcp"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="vivax_coef_max" & variable=="Prcp"),valor)), 
              alpha = 0.1, fill = "blue") +
  geom_ribbon(data = filter(df_betas2, categoria=="vivax_coef_min" & variable=="Soilm"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="vivax_coef_min" & variable=="Soilm"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="vivax_coef_max" & variable=="Soilm"),valor)), 
              alpha = 0.1, fill = "blue") +
  geom_ribbon(data = filter(df_betas2, categoria=="vivax_coef_min" & variable=="Tmax"), 
              mapping = aes(ymin = pull(filter(df_betas2, categoria=="vivax_coef_min" & variable=="Tmax"),valor), 
                            ymax = pull(filter(df_betas2, categoria=="vivax_coef_max" & variable=="Tmax"),valor)), 
              alpha = 0.1, fill = "blue") +
  geom_line(data = filter(df_betas2, categoria=="falci_coef"), 
            mapping = aes(y = pull(filter(df_betas2, categoria=="falci_coef"),valor), col = "falci_coef")) +
  geom_line(data = filter(df_betas2, categoria=="vivax_coef"),
            mapping = aes(y = pull(filter(df_betas2, categoria=="vivax_coef"),valor), col = "vivax_coef")) +
  ylab("Coeficientes") +
  geom_vline(xintercept = c(as.Date("2006/1/1"),as.Date("2010/12/1")), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(.~variable, scales = "free", nrow = 4, ncol = 1) +
  labs(colour = "Spp coef")

p1

ggplot(data = df_betas2, mapping = aes(x = fecha)) +
  geom_line(data = filter(df_betas2, categoria=="falci_coef"), mapping = aes(y = pull(filter(df_betas2, categoria=="falci_coef"),valor))) +
  geom_line(data = filter(df_betas2, categoria=="falci_coef_min"), mapping = aes(y = pull(filter(df_betas2, categoria=="falci_coef_min"),valor))) +
  geom_line(data = filter(df_betas2, categoria=="falci_coef_max"), mapping = aes(y = pull(filter(df_betas2, categoria=="falci_coef_max"),valor))) +
  facet_wrap(.~variable, scales = "free", nrow = 4, ncol = 1)

p1 <- ggplot(df_betas2, aes(x = fecha)) +
  geom_ribbon(data = filter(df_betas2, categoria==c("falci_coef_min","falci_coef_max")), aes(ymin = valor - 0.001, ymax = valor + 0.001), alpha = 0.5) +
  geom_line(data = filter(df_betas2, categoria=="falciparum"), aes(y = valor)) +
  geom_ribbon(data = filter(df_betas2, categoria=="vivax"), aes(ymin = valor - 0.001, ymax = valor + 0.001), alpha = 0.1) +
  geom_line(data = filter(df_betas2, categoria=="vivax"), aes(y = valor)) +
  ylab("Coeficientes") +
  geom_vline(xintercept = c(as.Date("2006/1/1"),as.Date("2010/12/1")), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(.~variable, scales = "free", nrow = 4, ncol = 1)

p1

p2 <- ggplot(df_betas2, aes(x = fecha)) +
  geom_line(data = filter(df_betas2, categoria=="promedio"), aes(y = valor)) + 
  ylab("Promedios") +
  geom_vline(xintercept = c(as.Date("2006/1/1"),as.Date("2010/12/1")), linetype = "dashed") +
  facet_wrap(.~variable, scales = "free", nrow = 4, ncol = 1)

p2  

ggarrange(p1, p2, ncol = 2, common.legend = T)


df_betas4 %>% 
  mutate(promedios = scales::rescale(promedios, to=range(df_betas4$vivax))) %>% 
  ggplot(aes(x = fecha)) +
  facet_wrap(.~categorias, scales = "free") +
  geom_line(aes(y = falciparum, col = "falciparum")) +
  geom_line(aes(y = vivax, col = "vivax")) +
  geom_line(aes(y = promedios, col = "promedios")) +
  scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(., to=range(df_betas4$promedios)),
                                         name = "Promedio"))

ggplot(df_betas2, aes(y = valor, x = fecha,col=categoria)) +
  geom_line() +
  geom_vline(xintercept = c(as.Date("2006/1/1"),as.Date("2010/12/1"))) +
  facet_wrap(.~variable + categoria, scales = "free", nrow = 4, ncol = 3)



###############################

# Agrupar por Distritos

myfunct<-function(d){
  slide_period_dfr(
    .x = d,
    .i = d$fecha,
    .period = "month",
    ~data.frame(
      Aet = coef(zeroinfl(formula = falciparum ~ aet, data = .x, dist = "negbin"))[[2]]
    ),
    .every = 12,
    .before = 0,
    .after = 0,
    .complete = T
  )
}

dt_final %>%
  group_by(NOMBPROV) %>% 
  summarise(
    slide_dfr(
      
    )
  )


df_betas[[5]] <- myfunct(df$data[[8]])

df_betas = vector(mode = "list", length = 49)

for(i in 1:49){
  df_betas[[i]] <- myfunct(df$data[[i]])
}

view(df$data[[3]])

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