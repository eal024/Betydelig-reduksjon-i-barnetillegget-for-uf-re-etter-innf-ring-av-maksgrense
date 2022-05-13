

# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)



df_inntekt_per_ar <- readxl::read_excel("data/2022-03-16 inntektsdata_per_ar_til_reg_figur.xlsx", sheet = 1) %>% 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))) %>% 
    mutate( across(  .cols = c(inntekt),  .fns = function(x)  x*106399) )




df_did_per_ar <- readxl::read_excel("data/2022-03-16 data_inntekt_og_did.xlsx", sheet = 1) %>% 
    mutate( across(  .cols = c(kat, kg),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(did, se),  .fns = function(x)  x*106399) )


# Graf DID --------------------------------------------------------------------

ps <- position_dodge( width = 0.7)

data_did <- df_did_per_ar %>% 
    mutate( keep = case_when( kat == "kontroll" & kg == 0.7              ~ 1,
                              kat == "ny kontroll og klustra" & kg == .8 ~ 1,
                              T ~0
                              )
            ) |> 
    filter( keep == 1 ) %>% 
    mutate( kg = ifelse(kg == "0.7", " ", "  "))


graf_did <- 
    data_did %>% 
    ggplot( aes( y = did, x = ar, fill = kat, color = kat ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    # geom_point( data = df_did_per_ar %>% 
    #                 mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    #                 filter( !str_detect(kat, "enkel|uten") ), aes( x = ar, y = did), color = "red",
    #             shape = 18,
    #             position = ps,
    #             size = 3
    #             ) +
    geom_errorbar( aes(ymin = did - se*1.96, ymax = did + se*1.96), position = ps, alpha  =.3, color = "black", width = .3) +
    geom_errorbar( data = data_did %>%
                       filter( kat == "ny kontroll og klustra", kg == "  ") %>% 
                       filter( ar %in% c(2018:2021)),
                   aes(x = ar, y = did, ymin = did - se*1.28, ymax = did + se*1.28),
                   position = ps, alpha  =.3, color = "red", width = .3, inherit.aes = F, size = 2 ) +
    facet_wrap( ~kg) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    #scale_y_continuous( labels = function(x) format(x, digits = 0, big.mark = " ") ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16)#,
           # text = element_text( family = "Times")
    ) +
    labs( y = "FiF-estimat") 


graf_did

df_did_per_ar %>% 
    filter( (kat ==   "kontroll") ) %>% 
    select(ar, kg,did) %>%
    distinct() %>% 
    pivot_wider( names_from = kg, values_from = did)



# Graf barnetillegg per år
df_inntekt_per_ar %>% 
    select(ar,gr, inntekt) %>% 
    distinct() %>% 
    pivot_wider( names_from = gr, values_from = inntekt) %>% 
    mutate( diff_0.7 = `[0.95,~)`-`[0.7,0.95)`,
            diff_0.8 = `[0.95,~)`-`[0.8,0.95)`
    )

graf_inntekt_pr_ar <- df_inntekt_per_ar %>% 
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
    scale_y_continuous( labels = function(x) format( x, big.mark = " ") ) +
    labs( y = "Inntekt",  caption = "Kilde: NAV")


graf_utprint <- graf_inntekt_pr_ar/graf_did


graf_utprint  %>% ggsave( filename = "plot/figur10.png", device = "png", width = 10, height =10 ) 


getwd()






