


# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)

# 
data_figur12 <- readxl::read_excel("data/2022-03-22 data_til_figur_12.xlsx", sheet = 1) %>% 
    mutate( alder = ifelse(fra == 0.949 & ar == 2015, 46.5,alder ))



data <- data_figur12 %>% 
    select(-c(fra,til)) %>% 
    pivot_longer(-c(gr,ar)) %>% 
    mutate( name = case_when(name == "bor_med"            ~  "Samboer/gift",
                             name == "NOR"                ~ "Norsk",
                             name == "ant_barn_felles"    ~  "Barn boende hos to foreldre",
                             name == "ant_barn_saerkull"  ~  "Barn boende hos en foreldre",
                             name == "ifuo"               ~"Oppjustert inntekt før uførhet",
                             name == "kjonn"              ~"Kvinne",
                             name == "teoretisk_kom_grad" ~ "KG, før avkorting",
                             name == "ant_barn" ~ "Antall barn",
                             
                             name == "alder" ~"Alder",
                             T ~ name)  %>% factor( levels = c(
                                 "Alder",
                                 "Kvinne",
                                 "Samboer/gift",
                                 "Antall barn",
                                 "Barn boende hos to foreldre",
                                 "Barn boende hos en foreldre",
                                 "Oppjustert inntekt før uførhet",
                                 "KG, før avkorting",
                                 "Norsk"
                             ))
    )
    
unique(data$name)

graf1 <-data %>%     #filter( gr == "[0%,95%)") %>% 
    ggplot( aes(x = ar, y = value, fill = as.factor(gr), color = as.factor(gr)) ) +
    geom_line( ) +
    facet_wrap( ~name, scales = "free")


graf1

# Graf --------------------------------------------------------------------

# Graf did

graf_covar <- 
    graf1 + 
    geom_point( aes(fill = gr, color = gr, shape = gr), color = "black", alpha =.8 ) +
    theme_ipsum( ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title = element_text(size=20,face="bold"),
           axis.title.x = element_text(size = 16),
           text = element_text( family = "Times")
    ) +
    labs( y = NULL, x = "År") 

graf_covar%>% ggsave( filename = "plot/figur12", device = "png", width = 10, height =10 ) 



