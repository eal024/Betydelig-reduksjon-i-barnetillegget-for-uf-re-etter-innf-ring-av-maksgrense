
library(tidyverse)
library(tidyverse)
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))
library(hrbrthemes)

df <- readxl::read_excel("data/2022-02-08 data_histogram_kg.xlsx")

# Histogram
df$teoretisk_kom_grad[df$ar == 2016] %>% hist()

dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
}

graf_histogram <- df %>%
    filter( ar %in% c(2016,2018,2021) ) %>%
    select(ar, teoretisk_kom_grad, for_2016) %>% 
    ggplot( aes( x = teoretisk_kom_grad , fill= as.factor( for_2016))) +
    geom_histogram( 
        # aes( y = ..ncount../sum(..ncount..) ),
        #     binwidth = 0.015, color = "white",
        #boundary = 0,
        #fill = "#FFA615",
        # alpha = 0.8 
        position = "identity",
        alpha = 0.5
    ) + 
    geom_vline( xintercept = 0.95, linetype = 2, color = "red") +
    facet_wrap( ~ar) +
    scale_x_continuous( breaks = c(.3, .6, .95, 1.3), labels = function(x) ifelse( x == 0.95, format(x, digits =2) %>% dropLeadingZero, format(x, digits =1) %>% dropLeadingZero ) ) +
    theme_ipsum() + 
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10),
           text = element_text( family = "Times")
           ) +
    labs( x = NULL, y = NULL ) +
    c


graf_histogram %>% ggsave( filename =  str_c("plot/", lubridate::today(), " figur2_histogram.png"), device = "png" , width = 7.5, height = 4)




