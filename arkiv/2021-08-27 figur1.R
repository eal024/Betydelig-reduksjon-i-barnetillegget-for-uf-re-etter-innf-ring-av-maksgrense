

library(tidyverse)
theme_set( theme_light())

data <- readxl::read_excel("data/2021-08-27 data_figur1.xlsx")


data_1 <- data %>% filter( index %in% c(1,5,6), 
                           ! name  %in% c("effekt", "komp_grad", "inntekt", "arbeidstid")
                               )


ggplot(data = data_1,
       aes( x = ar, y = value, fill = factor(grupper), color = factor(grupper), shape = factor(grupper) )
       ) +
    geom_point(size = 1.1) +
    geom_line( size = 1.1, alpha = 0.8) + 
    facet_wrap( ~name, scales = "free") +
    scale_x_continuous( breaks = seq(from = 2010, to = 2022, by = 2) , labels = function(x) format(x, digits = 0) ) +
    theme( legend.position =  "bottom" ,
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank()
           
    ) +
    geom_text( 
        aes( x = 2016, label = ifelse(ar == 2016 & name == "alder" , grupper, ""), size =1.1 ) ) +
    theme( legend.position =  "none" ,
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank()
           
    ) +
    labs( y = "Gj.stillingsandel, og inntekt i G (til høyre)" , x = "År") +
    geom_vline( xintercept = 2016, linetype = 2, color = "red", size = 1.2) +
    scale_x_continuous( breaks = c(2015,2017,2019, 2021) ,
                        labels = function(x) format(x, digits = 0) )


graf1

# ggsave(graf1, path = "plot", filename = "graf1.png", device = "png", height = 4, width = 12 )

