
# FiF som funksjon av valget av kontrollgruppe (KG)  

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

# Inntekt ---------------------------------------------------------------------

# data
df <- readxl::read_excel("data/2022-04-05 data_fif_funksjon_av_kg.xlsx", sheet = 1)

figur_fif_inntekt_av_kg <- df %>%
    ggplot( aes( y = coef, x = lim.nedre)) +
    geom_point() +
    geom_errorbar( aes(ymin = coef-st.error*1.94, ymax = coef+st.error*1.94, y = coef) ) +
    scale_x_continuous( labels = function(x)  format(x, digits =2) %>% dropLeadingZero  ) +
    scale_y_continuous( limits = c(-0.1, 0.35)) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) +
    geom_vline( xintercept = c(0.7,0.8), color = "red", linetype = 2) +
    facet_wrap( ~ar) +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10)
    ) +
    labs( x = "Kompensasjonsgrad, nedre grense", y = "FiF, i grunnbeløp")

figur_fif_inntekt_av_kg

figur_fif_inntekt_av_kg |> ggsave( filename =  str_c("plot/", lubridate::today(), " figur_fif_funksjon_av_kg.png"), device = "png" , width = 7.5, height = 6)



