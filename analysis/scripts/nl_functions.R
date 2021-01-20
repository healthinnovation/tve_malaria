# helpers for NL

## FUNCTION
library(scales)
library(tidyverse)
library(ggthemes)

dual_plot <- function(data, x, y_left, y2_left = NULL, y_right, group=NULL, col_left, col_right,
                       name_legend = " ", labels_legend = c("a","b")) {
  # original: https://stackoverflow.com/questions/56426472/dual-axis-plot-with-automatically-calculated-sec-axis-formula-in-ggplot2
  x <- ensym(x)
  y_left <- ensym(y_left)
  y_right <- ensym(y_right)
  
  if (!is.null(y2_left)) {y2_left <- ensym(y2_left)} else {y2_left <- y2_left}
  if (!is.null(group)) {group <- ensym(group)} else {group <- group}
  
  # Introducing ranges
  left_range <- range(data %>% pull(!!y_left))
  right_range <- range(data %>% pull(!!y_right))
  
  if (!is.null(group) & !is.null(y2_left)) {data1 <- data %>% select(!!x, !!y_left, !!y_right, !!group, !!y2_left)}
  if (!is.null(group) & is.null(y2_left)) {data1 <- data %>% select(!!x, !!y_left, !!y_right, !!group)}
  if (is.null(group) & !is.null(y2_left)) {data1 <- data %>% select(!!x, !!y_left, !!y_right, !!y2_left)}
  if (is.null(group) & is.null(y2_left)) {data1 <- data %>% select(!!x, !!y_left, !!y_right)}
  
  # Transform
  data1 %>%
    mutate(!!y_right := scales::rescale(!!y_right, to=left_range)) %>%
    gather(k, v, -!!x, -!!group) %>%
    ggplot() +
    geom_line(aes(!!x, v, colour = k, linetype=k)) +
    # Change secondary axis scaling and label
    scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(., to=right_range),
                                           name = rlang::as_string(y_right))) +
    {if(!is.null(y2_left)) {scale_color_manual(values = c(col_left,col_left,col_right))}
      else{scale_color_manual(values = c(col_left,col_right))}} +
    {if(!is.null(y2_left)) {scale_linetype_manual(breaks = c(y_left, y2_left), name = name_legend, 
                                                  values = c(3,2,1), labels = labels_legend)}
      else{scale_linetype_manual(values = c(2,1))}} +
    {if(!is.null(y2_left)) {guides(color = F)}
      else{guides(color = F, linetype = F)}} +
    theme_few() +
    theme(axis.title.y = element_text(color = col_left, size=13),
          #axis.line.y = element_line(color = col_left),
          axis.text.y = element_text(color = col_left),
          axis.ticks.y = element_line(color = col_left),
          axis.title.y.right = element_text(color = col_right, size=13),
          #axis.line.y.right = element_line(color = priceColor),
          axis.text.y.right = element_text(color = col_right),
          axis.ticks.y.right = element_line(color = col_right),
          legend.position = "top"
    ) +
    facet_wrap(vars(!!group), scales = "free")
}

df_promedios<-df_betas %>% 
  gather(categorias, promedios, c(Aet_mean,Prcp_mean,Soilm_mean,Tmax_mean)) %>% 
  select(c(categorias,promedios)) %>% 
  mutate(categorias = recode(categorias,"Aet_mean" = "Aet", "Prcp_mean" = "Prcp", "Soilm_mean" = "Soilm", "Tmax_mean" = "Tmax"))

df_falci<-df_betas %>% 
  gather(categorias, falciparum, c(Aet_falci,Prcp_falci,Soilm_falci,Tmax_falci)) %>% 
  select(c(categorias,falciparum)) %>% 
  mutate(categorias = recode(categorias,"Aet_falci" = "Aet", "Prcp_falci" = "Prcp", "Soilm_falci" = "Soilm", "Tmax_falci" = "Tmax"))

df_vivax<-df_betas %>% 
  gather(categorias, vivax, c(Aet_vivax,Prcp_vivax,Soilm_vivax,Tmax_vivax)) %>% 
  select(c(categorias, vivax)) %>% 
  mutate(categorias = recode(categorias,"Aet_vivax" = "Aet", "Prcp_vivax" = "Prcp", "Soilm_vivax" = "Soilm", "Tmax_vivax" = "Tmax"))
  
cbind(df_falci,df_vivax$vivax,df_promedios$promedios) %>% 
  add_column(
    fecha = rep(seq(as.Date("2000/1/1"), by = "month", length.out = 205),4)
  ) %>% 
  mutate(!!ensym(df_promedios$promedios) := scales::rescale(!!df_promedios$promedios, to=range(data %>% pull(!!falciparum)))) %>% 
  gather(k, v, -!!fecha, -!!categorias) %>%
  ggplot() +
  geom_line(aes(!!fecha, v, colour = k, linetype=k)) +
  scale_y_continuous(sec.axis = sec_axis(~ scales::rescale(., to=range(data %>% pull(!!df_promedios$promedios))),
                                         name = rlang::as_string(df_promedios$promedios))) +
  scale_color_manual(values = c("blue","blue","red")) +
  scale_linetype_manual(breaks = c(falciparum, df_vivax$vivax), name = " ", 
                        values = c(3,2,1), labels = c("a","b")) +
  guides(color = F) +
  theme_few() +
  theme(axis.title.y = element_text(color = "blue", size=13),
        #axis.line.y = element_line(color = col_left),
        axis.text.y = element_text(color = "blue"),
        axis.ticks.y = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "red", size=13),
        #axis.line.y.right = element_line(color = priceColor),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        legend.position = "top"
  ) +
  facet_wrap(vars(!!group), scales = "free")
  
  
df_betas4 <- cbind(df_falci,df_vivax$vivax,df_promedios$promedios) %>% 
  add_column(
    fecha = rep(seq(as.Date("2000/1/1"), by = "month", length.out = 205),4)
  ) %>% 
  rename(vivax = 'df_vivax$vivax', promedios = 'df_promedios$promedios')
  
df_betas4 %>% 
  dual_plot(x = fecha, y_left = falciparum, y_right = promedios, y2_left = "vivax", group = "categorias", col_left = "red", col_right = "blue")


