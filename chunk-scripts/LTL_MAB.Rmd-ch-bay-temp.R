
ecodata::ch_bay_temp %>%
  tidyr::pivot_wider(names_from = Var, values_from = Value) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(aes(x = Time, ymin = minLTA, ymax = maxLTA), fill = "grey", alpha = 0.5)+
  
  ggplot2::geom_line(aes(x = Time, y = YearLTA, color= "Long Term Average 2010-2019")) +
  ggplot2::geom_line(aes(x = Time, y = Year2020, color = "Daily 2020")) +
  ggplot2::ylab("Temperature (F)") +
  ggplot2::ggtitle("Chesapeake Bay Temperature") +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
