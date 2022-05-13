

# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)



df_inntekt_per_ar_placbo <- 
    readxl::read_excel("data/2022-03-17 placebo_data.xlsx", sheet = "verdi") %>% 
    mutate( across( .cols = c(gr), .fns = function(x) factor(x))) %>% 
    mutate( across(  .cols = c(barntillegg, inntekt),  .fns = function(x)  x*106399) ) %>% 
    pivot_longer( -c(ar,gr) ) %>% 
    arrange(name, gr, ar)




df_did_per_ar_placbo <-     
    readxl::read_excel("data/2022-03-17 placebo_data.xlsx", sheet = "did") %>% 
    mutate( across( .cols = c(gr), .fns = function(x) factor(x))) %>% 
    mutate( across(  .cols = c(estimate, str_e),  .fns = function(x)  x*106399) )


# Graf --------------------------------------------------------------------

# Graf did

ps <- position_dodge( width = 0.7)

graf_did<- df_did_per_ar_placbo %>%
    rename( kat = gr, did = estimate, se = str_e) %>% 
    mutate( kg = "Kontrollgruppe: [80%,95%), Berørt: [95%,øvre grense)") %>% 
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
    facet_wrap( ~kat) +
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

graf_did

graf_inntekt_pr_ar_placbo <- df_inntekt_per_ar_placbo %>% 
    #filter( name == "barntillegg") %>% 
    #mutate( kg = ifelse(kg == "0.7", "Kontrolgruppe: [70%,95%)", "Kontrolgruppe:[80%,95%)")) %>% 
    ggplot( aes( y = value , x = ar, color = gr, fill = gr)) +
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
    facet_wrap(~name, scales = "free_y") +
    scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark = " ") ) +
    scale_x_continuous( breaks = seq(from = 2016, to = 2021, by = 2) ) +
    labs( y = "Inntekt", x = "År")

graf_inntekt_pr_ar_placbo

graf_utprint <- graf_inntekt_pr_ar_placbo/graf_did

graf_utprint %>% ggsave( filename = "plot/graf_did_placebo.png", device = "png", width = 10, height =10 )













