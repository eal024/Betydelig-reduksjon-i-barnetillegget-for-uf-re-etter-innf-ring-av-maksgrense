# Pakker
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)


dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
}



# Figur med inntekt og bt som y og KG som x -------------------------------

# y-aksen inntekt, x-aksen KG
df <- readxl::read_excel("data/2022-04-05 figur3_xakse_kg_y.xlsx", sheet = 1)

vline <- tibble( ar   = rep( c(2016:2021), each = 1),
                 line = seq(from = 0.95, to = 1.10, by = 0.03) |> rev()
)


#Graf
graf3_bt_utbetalt <- df %>%
    filter( antall > 15, ar > 2015) %>%
    ggplot( aes(y = barnetillegg, x = kg, fill = overg_rlg, color = overg_rlg) ) +
    geom_point( alpha = .6) +
    geom_smooth( se = F) +
    geom_vline( xintercept = c(0.95)) +
    geom_vline( data = vline, aes( xintercept = line), linetype = 2, color = "red") +
    scale_x_continuous( labels = function(x)  format(x, digits =2) %>% dropLeadingZero  ) +
    facet_wrap( ~ar) +
    labs( y = "Utbetalt barnetillegg", x = "Kompensasjonsgrad, før avkorting") +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10)
    ) 


graf3_bt_utbetalt |> ggsave( filename =  str_c("plot/", lubridate::today(), " figur_3_utb_bt.png"), device = "png" , width = 7.5, height = 6)



# Barnetillegget ----------------------------------------------------------


graf3_inntekt <- df %>%
    filter(   antall > -1, inntekt > 0, ar > 2015) %>%
    ggplot( aes(y = inntekt, x = kg, fill = overg_rlg, color = overg_rlg) ) +
    geom_point( alpha = .8) +
    geom_smooth( se = F) +
    scale_y_continuous( limits = c(0,2)) +
    geom_vline( xintercept = c(0.95)) +
    geom_vline( data = vline, aes( xintercept = line), linetype = 2, color = "red") +
    facet_wrap( ~ar) +
    labs( y = "Inntekt fra arbeid", x = "Kompensasjonsgrad, før avkorting") +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10)
    ) 

graf3_inntekt |> ggsave( filename =  str_c("plot/", lubridate::today(), " figur_3_inntekt.png"), device = "png" , width = 7.5, height = 6)

