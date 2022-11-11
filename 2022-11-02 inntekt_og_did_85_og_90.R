
# oppdatert figur per 20.7.22

# Data og pakker ----------------------------------------------------------

Sys.getlocale()
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)


# Inntektsdata
df_inntekt_per_ar <- readxl::read_excel("data/2022-03-16 inntektsdata_per_ar_til_reg_figur.xlsx", sheet = "inntekt_mottakere") |>  
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))) |>  
    mutate( across(  .cols = c(inntekt),  .fns = function(x)  1*x*111477) )





data_did_inntekt <- readxl::read_excel("data/2022-03-16 data_inntekt_og_did.xlsx", sheet = "regresjon_inntekt") %>% 
    rename( `Ar` = 2) |> 
    mutate( across(  .cols = c(gr),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(Estimat, st.avvik),  .fns = function(x)  x*12*111477) ) 




# Graf DID --------------------------------------------------------------------

ps <- position_dodge( width = 0.7)

data_did <- data_did_inntekt #%>% 
# mutate( keep = case_when( kat == "kontroll" & kg == 0.7              ~ 1,
#                           kat == "ny kontroll og klustra" & kg == .8 ~ 1,
#                           T ~0
# )
# ) |> 
# filter( keep == 1 ) %>% 
# mutate( kg = ifelse(kg == "0.7", " ", "  "))


graf_did_inntekt <- data_did_inntekt %>% 
    #filter( gr == "0.85") |> 
    add_row( gr = c("0.85", "0.9"), Ar = 2015, Estimat = NA_real_, st.avvik = NA_real_  ) |> 
    mutate( gr = ifelse( as.character(gr) == "0.85", "85%", "95%") ) |> 
    ggplot( aes( y = Estimat, x = Ar, fill = gr, color = gr ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    # geom_point( data = df_did_per_ar %>% 
    #                 mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    #                 filter( !str_detect(kat, "enkel|uten") ), aes( x = ar, y = did), color = "red",
    #             shape = 18,
    #             position = ps,
    #             size = 3
    #             ) +
    geom_errorbar( aes(ymin = Estimat - st.avvik*1.96, ymax = Estimat + st.avvik*1.96), position = ps, alpha  =.3, color = "black", width = .3,
                   size = 1) +
    # geom_errorbar( data = data_did  ,
    #                aes(x = ar, y = Estimat, ymin = Estimat - se*1.28, ymax = Estimat + se*1.28),
    #                position = ps, alpha  =.3, color = "red", width = .3, inherit.aes = F, size = 2 ) +
    #facet_wrap( ~gr) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    expand_limits( x = c(2015, 2021)) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16)#,
           # text = element_text( family = "Times")
    ) +
    labs( y = "FiF-estimat", x = " " ) +
    facet_wrap(~ gr)


graf_did_inntekt

graf_inntekt_pr_ar <- 
    df_inntekt_per_ar  |>  
    mutate( kg = ifelse( as.character(kg) == "0.85", "Kontrollgruppe [85%,95%)","Kontrollgruppe [90%,95%)" )) |> 
    ggplot( aes( y = inntekt , x = ar, color = gr, fill = kg)) +
    geom_point(aes(shape = gr), size = 2) +
    geom_line( alpha =.5 , size = 1) +
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
    #facet_wrap(~kg) +
    scale_x_continuous( breaks =  seq(2015,2021, by = 2)) +
    scale_y_continuous( labels = function(x) format( x, big.mark = " ") ) +
    labs( y = "Inntekt", x = " ",  caption = "Kilde: NAV") +
    facet_wrap(~kg)


graf_inntekt_pr_ar

# Del i to figurer. 
graf_utprint <- (graf_inntekt_pr_ar)/graf_did_inntekt


graf_utprint  %>% ggsave( filename = "plot/figurV2.png", device = "png", width = 10, height =10 ) 
graf_utprint  %>% ggsave( filename = "plot/SVG/figurV2.svg", device = "svg", width = 10, height =10 ) 






