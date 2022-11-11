
library(tidyverse)
library(tidyverse)
library(extrafont)
library(hrbrthemes)
windowsFonts(Times=windowsFont("TT Times New Roman"))


# Data
df <- readxl::read_excel("data/2022-02-08 data_histogram_kg.xlsx")

# Histogram: kjapt år 2016
df$teoretisk_kom_grad[df$ar == 2016] %>% hist()

# Funksjon, ta bort 0 før . i 0.95
dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
}

# Histogram år 2016-2021
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
    #scale_fill_manual(values = c(  "#9D5A6C", "#9D5A7C")) +
    #scale_fill_manual(values = c( "yellow4", "green4" )) +
    #theme_niwot() +
    guides(fill = FALSE, color = FALSE) +
    labs( x = NULL, y = NULL ) 



graf_histogram %>% ggsave( filename =  str_c("plot/", lubridate::today(), " figur2_histogram.png"), device = "png" , width = 7.5, height = 4)
graf_histogram %>% ggsave( filename =  str_c("plot/SVG/", lubridate::today(), " figur2_histogram.svg"), device = "svg" , width = 7.5, height = 4)



