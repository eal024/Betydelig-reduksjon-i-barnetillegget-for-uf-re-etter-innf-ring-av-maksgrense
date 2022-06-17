

library(tidyverse)

# Figur 3b: Aim is to show that income increases when individs are treated.


df <- readxl::read_excel("data/2022-02-11 data_figur3.xlsx")



df %>% 
    filter( ar %in% c(2016, 2018, 2020),
            between( kg, 0.8, 1.2)
            ) %>% 
    mutate( for_2016 = as.factor(for_2016)) %>% 
    ggplot( aes( y = inntekt, x = kg, fill = for_2016, color = for_2016 , shapes = for_2016)) +
    geom_point( ) +
    geom_line( alpha = 0.2) +
    geom_smooth( data = df %>% filter( ar %in% c(2016,2018,2020),between(kg, 0.9,.95), for_2016 == 0 ), method = "lm" , aes(x = kg, y = inntekt)  , se = F , inherit.aes = F) +
    geom_smooth( data = df %>% filter( ar %in% c(2016,2018,2020),between(kg, 0.95, 1.06), for_2016 == 0 ), method = "lm", aes(x = kg, y = inntekt)  , se = F , inherit.aes = F) +
    geom_vline( xintercept = 0.95) +
    facet_wrap( ~ar, ncol = 1 ) 
