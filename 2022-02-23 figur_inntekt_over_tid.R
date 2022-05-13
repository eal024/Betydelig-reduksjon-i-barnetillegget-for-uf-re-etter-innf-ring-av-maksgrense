


# Pakker og data
library(tidyverse)
library(hrbrthemes)
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))
loadfonts(dev="win")

# Data
df <- readxl::read_excel("data/2022-02-23 data_inntekt.xlsx", sheet = 1)


graf_inntekt <- df %>% 
    #mutate( for_2016 = ifelse(for_2016 == 1, paste0("Del av overgangsordning"), paste0("Nye mottaker etter 2016")  ) %>% as.factor() ) %>% 
    filter( id == "Mottaker") %>% 
    mutate( inntekt = inntekt*106399) %>% 
    ggplot( aes(x = ar, y = inntekt, fill = as.factor(gr), color= as.factor(gr)) ) +
    geom_line( size = 1, alpha = 0.8) +
    geom_point( size = 2, aes(shape = factor(gr) )) + 
    #facet_wrap(~ id, scales = "free_y") +
    theme_ipsum(  ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 12),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 12),
           text = element_text( family = "Times" )
    ) +
    scale_fill_manual(values = c(  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c( "#EBB261", "#9D5A6C")) +
    scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark= " ")) +
    scale_x_continuous( breaks = seq(from =2015, to = 2021, by =1)) +
    labs( y = "Inntekt fra arbeid", x = "År") +
    geom_text( data = tibble( y = c(49000,28000),
                       x = 2016,
                       gr = c(0,1),
                       text = c("[0,95%)",
                                "[95%,~)"
                                )
                       ),
               aes(y = y, x = x, label = text), size = 4
               )


graf_inntekt %>% ggsave( filename = "plot/figur7_inntekt.png", device = "png", width = 6.5, height =4)

graf_inntekt_hus <- df %>% 
    #mutate( for_2016 = ifelse(for_2016 == 1, paste0("Del av overgangsordning"), paste0("Nye mottaker etter 2016")  ) %>% as.factor() ) %>% 
    filter( id == "Husholdning") %>% 
    mutate( inntekt = inntekt*106399) %>% 
    ggplot( aes(x = ar, y = inntekt, fill = as.factor(gr), color= as.factor(gr)) ) +
    geom_line( size = 1, alpha = 0.8) +
    geom_point( size = 2, aes(shape = factor(gr) )) + 
    #facet_wrap(~ id, scales = "free_y") +
    theme_ipsum(  ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           text = element_text( family = "Times")
    ) +
    scale_fill_manual(values = c(  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c( "#EBB261", "#9D5A6C")) +
    scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark= " ")) +
    scale_x_continuous( breaks = seq(from =2015, to = 2021, by = 2)) +
    labs( y = "Inntekt fra arbeid", x = "År") 

graf_inntekt_hus %>% ggsave( filename = "plot/graf_inntekt_hus.png", device = "png", width = 6.5, height =3.5)

