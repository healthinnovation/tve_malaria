# helpers for NL

## FUNCTION
library(scales)
library(tidyverse)

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
  
  if (is.null(group)) {data2 <- data1 %>% mutate(!!y_right := scales::rescale(!!y_right, to=left_range))}
  if (!is.null(group)) {data2 <- data1 %>% mutate(gg := !!group) %>%
    group_by(gg) %>% mutate(!!y_right := scales::rescale(!!y_right, to=left_range))}
  
  # Transform
  data2 %>%
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
