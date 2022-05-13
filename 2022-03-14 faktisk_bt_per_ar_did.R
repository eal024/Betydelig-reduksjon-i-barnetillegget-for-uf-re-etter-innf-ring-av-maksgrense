


# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)



df_bt_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = 1) %>% 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))    ) %>% 
    mutate( across(  .cols = c(barnetillegg),  .fns = function(x)  x*106399) 
            )
    
df_did_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = 2) %>% 
    mutate( across(  .cols = c(kat, kg),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(did,se),  .fns = function(x)  x*106399) 
            )


# Graf --------------------------------------------------------------------

df_did_per_ar %>% 
    filter( str_detect(kat, "kontro") ) %>% 
    select(ar, kg,did) %>% 
    pivot_wider( names_from = kg, values_from = did)

# Graf did

ps <- position_dodge( width = 0.7)

graf_did <- 
    df_did_per_ar %>% 
    filter( kat == "enkel") %>% 
    mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    ggplot( aes( y = did, x = ar, fill = kat, color = kat ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    geom_errorbar( aes(ymin = did - se*1.65, ymax = did + se*1.65), position = ps, alpha  =.3, color = "black", width = .3) +
    facet_wrap( ~kg) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16),
           text = element_text( family = "Times")
    ) +
    scale_y_continuous( labels =  function(x) format( x , big.mark = " ") %>% str_c(., " kr")) +
    labs( y = "FiF-estimat", 
          x = "",
          caption = "kilde: Nav")

graf_did



# Faktisk utbetalt barnetillegg over år.
df_bt_per_ar %>% 
    select(ar,gr, barnetillegg) %>% 
    distinct() %>% 
    pivot_wider( names_from = gr, values_from = barnetillegg) %>% 
    mutate( diff_0.7 = `[0.95,~)`-`[0.7,0.95)`,
            diff_0.8 = `[0.95,~)`-`[0.8,0.95)`
            )

# Graf barnetillegg per år

graf_pr_ar <- df_bt_per_ar %>% 
    mutate( kg = ifelse(kg == "0.7", "Kontrolgruppe: [70%,95%)", "Kontrolgruppe:[80%,95%)")) %>% 
    ggplot( aes( y = barnetillegg, x = ar, color = gr, fill = kg)) +
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
    facet_wrap(~kg) +
    labs( y = "Utbetalt barnetillegg", x = "År") +
    scale_y_continuous( labels =  function(x) format( x,  big.mark = " ") %>% str_c(., " kr"))


graf_utprint <- graf_pr_ar/graf_did


graf_utprint %>% ggsave( filename = "plot/graf_did_og_per_ar_bt.png", device = "png", width = 10, height =10 )





# Cluster fk_person1 ------------------------------------------------------




# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)



df_bt_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = 1) %>% 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))    ) %>% 
    mutate( across(  .cols = c(barnetillegg),  .fns = function(x)  x*106399) 
    )

df_did_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = 3) %>% 
    mutate( across(  .cols = c(kat, kg),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(estimate, str.e),  .fns = function(x)  x*106399) 
    ) |> 
    rename( did = estimate,
            se = str.e)


# Graf --------------------------------------------------------------------

df_did_per_ar %>% 
    filter( str_detect(kat, "kontro") ) %>% 
    select(ar, kg,did) |> 
    group_by( kg) |> 
    mutate( vekst  = did - lag(did) ) |> 
    summarise( vekst = mean(vekst, na.rm = T))

# Graf did

ps <- position_dodge( width = 0.7)

graf_did <- 
    df_did_per_ar %>% 
    mutate( ar = as.numeric(ar)) |> 
    filter( kat == "kontroll") %>% 
    mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>% 
    ggplot( aes( y = did, x = ar, fill = kat, color = kat ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    geom_errorbar( aes(ymin = did - se*1.96, ymax = did + se*1.96), position = ps, alpha  =.3, color = "black", width = .3) +
    facet_wrap( ~kg) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 10),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16),
           text = element_text( family = "Times")
    ) +
    scale_x_continuous( breaks = seq( from = 2016, to = 2021, by =1) ) +
    scale_y_continuous( labels =  function(x) format( x,   big.mark = " ") %>% str_c(., " kr")) +
    labs( y = "FiF-estimat", x = "" , caption = "kilde: Nav")

graf_did



# Faktisk utbetalt barnetillegg over år.
df_bt_per_ar %>% 
    select(ar,gr, barnetillegg) %>% 
    distinct() %>% 
    pivot_wider( names_from = gr, values_from = barnetillegg) %>% 
    mutate( diff_0.7 = `[0.95,~)`-`[0.7,0.95)`,
            diff_0.8 = `[0.95,~)`-`[0.8,0.95)`
    )

# Graf barnetillegg per år

graf_pr_ar <- df_bt_per_ar %>% 
    mutate( kg = ifelse(kg == "0.7", "Kontrolgruppe: [70%,95%)", "Kontrolgruppe:[80%,95%)")) %>% 
    ggplot( aes( y = barnetillegg, x = ar, color = gr, fill = kg)) +
    geom_point(aes(shape = gr), size = 2) +
    geom_line( alpha =.5) +
    theme_ipsum( ) +ww
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
    facet_wrap(~kg) +
    labs( y = "Utbetalt barnetillegg" ) +
    scale_y_continuous( labels =  function(x) format( x, big.mark = " ") %>% str_c(., " kr"))


graf_utprint <- graf_pr_ar/graf_did



graf_utprint %>% ggsave( filename = "plot/figur9", device = "png", width = 10, height =10 ) 








