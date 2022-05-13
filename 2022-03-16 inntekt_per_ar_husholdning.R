

# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)



df_inntekt_per_ar_hus <- 
    readxl::read_excel("data/2022-03-16 inntektsdata_per_ar_til_reg_figur.xlsx", sheet = "husholdning") %>% 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))) %>% 
    mutate( across(  .cols = c(inntekt),  .fns = function(x)  x*106399) )




df_did_per_ar_hus <- readxl::read_excel("data/2022-03-16 data_inntekt_og_did.xlsx", sheet = "husholdning") %>% 
    mutate( across(  .cols = c(kat, kg),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(did, se),  .fns = function(x)  (x )*106399) )


# Graf --------------------------------------------------------------------

# Graf did

ps <- position_dodge( width = 0.7)

graf_did_hus <- df_did_per_ar_hus %>%
    filter( !str_detect(kat, "enkel|uten") ) %>% 
    mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    ggplot( aes( y = did, x = ar, fill = kat, color = kat ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    # geom_point( data = df_did_per_ar_hus %>% 
    #                 mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    #                 filter( !str_detect(kat, "enkel|uten") ), aes( x = ar, y = did), color = "red",
    #             shape = 18,
    #             position = ps,
    #             size = 3
    # ) +
    geom_errorbar( aes(ymin = did - se*2.1, ymax = did + se*2.1), position = ps, alpha  =.3, color = "black", width = .3) +
    facet_wrap( ~kg) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark = " ") ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16),
           text = element_text( family = "Times")
    ) +
    labs( y = "FiF-estimat", x = "År") 

graf_did_hus

graf_inntekt_pr_ar_hus <- df_inntekt_per_ar_hus %>% 
    mutate( kg = ifelse(kg == "0.7", "Kontrolgruppe: [70%,95%)", "Kontrolgruppe:[80%,95%)")) %>% 
    ggplot( aes( y = inntekt , x = ar, color = gr, fill = kg)) +
    geom_point(aes(shape = gr), size = 2) +
    geom_line( alpha =.5) +
    theme_ipsum( ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title=element_text(size=14,face="bold"),
           axis.title.y = element_text(size = 14),
           text = element_text( family = "Times")
    ) +
    scale_color_manual( values = c(navR::nav_farger()[4],navR::nav_farger()[1],navR::nav_farger()[2]  )  ) +
    #lims( y = c(0.1, .5)) +
    facet_wrap(~kg) +
    scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark = " ") ) +
    labs( y = "Inntekt", x = "År")


graf_utprint_hus <- graf_inntekt_pr_ar_hus/graf_did_hus

graf_utprint_hus %>% ggsave( filename = "plot/graf_did_husholdning.png", device = "png", width = 10, height =10 )













