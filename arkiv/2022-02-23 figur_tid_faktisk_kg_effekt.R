

library(tidyverse)
library(hrbrthemes)
windowsFonts(Times=windowsFont("TT Times New Roman"))


df <- readxl::read_excel("data/2022-02-23 figurer_tid_kg_effekt.xlsx", sheet = 1)


graf_11 <- 
    df %>% 
    mutate( for_2016 = ifelse(for_2016 == 1, paste0("Del av overgangsordning"), paste0("Nye mottaker etter 2016")  ) %>% as.factor() ) %>% 
    ggplot( aes(x = ar, y = kg, fill = as.factor(eff), color= as.factor(eff)) ) +
    geom_line( size = 1) +
    facet_wrap(~ for_2016) +
    theme_ipsum(  ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 16),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 16),
           text = element_text( family = "Times")
    ) +
    ylim( c(0.5,1)) +
    scale_fill_manual(values = c(  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c( "#EBB261", "#9D5A6C")) +
    labs( y = "Faktisk kompensasjonsgrad", x = "År") 

5
graf_11 %>% ggsave( filename = "plot/graf_11.png", device = "png", width = 8, height =4.5 )
