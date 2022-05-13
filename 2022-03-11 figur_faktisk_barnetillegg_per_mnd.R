

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))

# data
df <- readxl::read_excel("data/2022-03-11 data_bt_faktisk_per_mnd.xlsx") %>% 
    mutate( dato = as.Date(dato))


unique(df$kategori)

# 
df %>%
    mutate( keep = ifelse( (  str_detect(kategori, "nye etter") & dato < ymd("2016-02-01")) , F, T ),
            kategori = fct_rev(kategori)
    ) %>%
    filter( keep == T, 
            kategori %in% c("overgangordning, 12 mnd") 
    ) %>% 
    ggplot( aes( y = barnetillegg, x = dato , fill = factor(gr), color = factor(gr) ) ) +
    geom_vline( xintercept = seq(from = as.Date("2016-01-01"), to = as.Date("2022-01-01"), by = "year" ), 
                alpha = 0.5,
                linetype = 2,
                color = "black"
    ) +
    geom_line() +
    theme_ipsum() +
    theme( legend.position = "bottom") +
    labs( y = "Faktisk utbetalt barnetillegg", x = "År")

df %>%
    mutate( keep = ifelse( (  str_detect(kategori, "nye etter") & dato < ymd("2016-02-01")) , F, T ),
            kategori = fct_rev(kategori)
    ) %>%
    filter( keep == T, 
            str_detect(kategori,"nye etter 2016, alle") 
    ) %>% 
    ggplot( aes( y = barnetillegg, x = dato , fill = factor(gr), color = factor(gr) ) ) +
    geom_vline( xintercept = seq(from = as.Date("2016-01-01"), to = as.Date("2022-01-01"), by = "year" ), 
                alpha = 0.5,
                linetype = 2,
                color = "black"
    ) +
    geom_line() +
    theme_ipsum() +
    theme( legend.position = "bottom") +
    labs( y = "Faktisk utbetalt barnetillegg", x = "År")


# Data
df1 <- df %>%
    mutate( keep = ifelse( (  str_detect(kategori, "nye etter") & dato < ymd("2016-02-01")) , F, T ),
            kategori = fct_rev(kategori)
    
            ) %>%
    filter( keep == T, 
            kategori %in% c("nye etter 2016, alle","overgangordning, alle") 
    ) %>% 
    mutate( kategori = ifelse( str_detect(kategori, "nye etter"), "Nye mottakere etter 2016", "Mottakere i overgangsordning") ) 


text <- tibble( gr = df1$gr %>% unique() %>% rep(2),
                barnetillegg = c( c(0.23,0.32,0.4,.48 ),c(0.2,0.3,0.4,0.7) ),
                dato = c("2017-01-01","2018-01-01","2018-01-01","2018-01-01", "2017-01-01","2018-01-01","2018-01-01","2018-01-01") %>% ymd(),
                kategori = rep( c("Nye mottakere etter 2016","Mottakere i overgangsordning"), each = 4) ) 
                    

    
graf <- df1 %>%  
    ggplot( aes( y = barnetillegg, x = dato , fill = factor(gr), color = factor(gr) ) ) +
    geom_point( data = df1 %>% filter( month(dato) == 1),  aes( y = barnetillegg, x = dato , fill = factor(gr)), color ="black", size =2, alpha =.5 ) +
    geom_line( size = 0.8, alpha = .7) +
    theme_ipsum() +
    theme( 
           legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 14),
           text = element_text( family = "Times")) +
    labs( y = "Faktisk utbetalt barnetillegg", x = "År") +
    scale_color_manual( values = c(navR::nav_farger()[1],navR::nav_farger()[2],navR::nav_farger()[4],navR::nav_farger()[5] )  ) +
    geom_text( data =text, aes(y = barnetillegg, x = dato, label = gr), vjust = .1, hjust =.1 ) +
    facet_wrap( ~ kategori ) 



graf %>% ggsave( filename = "plot/graf_did_faktisk_bt.png", device = "png", width = 10, height =6 )



# Per år ------------------------------------------------------------------

df1 %>% 
    group_by( ar = year(dato), gr) %>% 
    summarise( bt = mean(barnetillegg) ) %>% 
    pivot_wider( names_from = gr, values_from = bt)
    





