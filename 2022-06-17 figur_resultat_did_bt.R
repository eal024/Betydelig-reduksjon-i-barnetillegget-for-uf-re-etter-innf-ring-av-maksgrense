
# Oppdatert per 20.7.22

# Data og pakker ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)


# Faktisk barnetillegg. Fra filen:
df_bt_pr_ar <-  readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = "faktisk_bt") |> 
    rename( barnetillegg = utb_bt) |> 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))    ) %>% 
    mutate( across(  .cols = c(barnetillegg),  .fns = function(x)  x*106399) 
    )

# DID-resultatet model2, (2022-06-13 replika data). Kontrollgruppen 85% 
df_bt_did_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = "oppdatert_modell") |>  
    mutate( across(  .cols = c(kg),  .fns = function(x) factor(x)) ) %>% 
    mutate( across(  .cols = c(estimate,str.e),  .fns = function(x)  x*106399) 
    )


# Graf --------------------------------------------------------------------

# Graf did

ps <- position_dodge( width = 0.7)

graf_did <- 
    df_bt_did_ar %>% 
    filter( kg == "0.85") |> 
    mutate( kg = ifelse( as.character(kg) == "0.85", "85%-95%", "90-95%" )) |> 
    ggplot( aes( y = estimate, x = ar, fill = kg, color = kg ) ) +
    geom_point( position = ps, size =3, color = "black" ) +
    geom_errorbar( aes(ymin = (estimate - str.e*1.96), ymax = (estimate + str.e*1.96) ), position = ps, alpha  =.3, color = "black", width = .3) +
    facet_wrap( ~kg) +
    geom_hline( yintercept = 0, color = "red", linetype = 2) + 
    theme_ipsum( ) +
    theme( legend.position = "none",
          #strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 10),
           axis.title=element_text(size=20,face="bold"),
           axis.title.y = element_text(size = 16),
           text = element_text( family = "Times")
    ) +
    scale_y_continuous( labels =  function(x) format( x , big.mark = " ") %>% str_c(., " kr"), limits = c(-35000,0)) +
    scale_x_continuous( breaks = seq( from = 2015, to = 2022, by = 2), limits = c(2015,2021.2)) +
    labs( y = "FiF-estimat", 
          x = "",
          title = "" #,
          #caption = "kilde: Nav"
          )

graf_did




# Graf barnetillegg per år
graf_pr_ar <- df_bt_pr_ar %>% 
    filter(kg == "0.8") |> 
    mutate( kg = ifelse(kg == "0.8", "Kontrolgruppe: [85%,95%)", "Kontrolgruppe:[90%,95%)")) %>% 
    ggplot( aes( y = barnetillegg, x = ar, color = gr, fill = kg)) +
    geom_point(aes(shape = gr), size = 2) +
    geom_line( alpha =.5) +
    theme_ipsum( ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 12),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 10),
           axis.title=element_text(size=14,face="bold"),
           axis.title.y = element_text(size = 14),
           text = element_text( family = "Times")
    ) +
    scale_color_manual( values = c(navR::nav_farger()[4],navR::nav_farger()[1],navR::nav_farger()[3]  )  ) +
    facet_wrap(~kg) +
    labs( x = NULL ,y = "Utbetalt barnetillegg",  caption = "Kilde:Nav") +
    scale_y_continuous( labels =  function(x) format( x,  big.mark = " ") %>% str_c(., " kr"), limits = c(25000,100000)) +
    scale_x_continuous( breaks = seq( from = 2015, to = 2021, by = 2), limits = c(2015, 2021.2))


graf_utprint <- graf_pr_ar/graf_did


graf_utprint %>% ggsave( filename = "plot/ny_graf_did_og_per_ar_bt.png", device = "png", width = 10, height =10 )
graf_utprint %>% ggsave( filename = "plot/SVG/ny_graf_did_og_per_ar_bt.svg", device = "svg", width = 7, height =7)


# 
# 
# # Inntekt col------------------------------------------------------

df_inntekt_col <- readxl::read_excel("data/2022-09-23 inntekt_col.xlsx")

graf_inntekt_col <- 
    df_inntekt_col |> 
    ggplot( aes( x = inntekt_c*111477, y = andel, fill = gr ) ) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous( labels = scales::percent) +
    scale_x_continuous( labels = scales::format_format( big.mark = " ") ) +
    scale_fill_manual(values=c("green4", "#ff9100")) +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10),
           text = element_text( family = "Times") #,
           #           axis.text.y=element_blank()
    )+ 
    labs( x = "Registrert inntekt i A-ordningen", y = "% fordeling", caption = "Kilde: Nav") 

ggsave( plot = graf_inntekt_col, filename =  "plot/SVG/graf_inntekt_fordeling.svg", device = "svg", height = 4, width = 6)




# # Data og pakker ----------------------------------------------------------
# 
# library(lubridate)
# library(tidyverse)
# library(hrbrthemes)
# library(extrafontdb)
# library(extrafont)
# library(extrafont)
# library(patchwork)
# 
# 
# 

readxl::excel_sheets("data/2022-03-14 data_faktisk_barnetillegg.xlsx")

df_bt_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = "faktisk_bt") %>%
    rename( barnetillegg = utb_bt) |> 
    mutate( across( .cols = c(gr, kg), .fns = function(x) factor(x))    ) %>%
    mutate( across(  .cols = c(barnetillegg),  .fns = function(x)  x*111477)
    )

df_did_per_ar <- readxl::read_excel("data/2022-03-14 data_faktisk_barnetillegg.xlsx", sheet = "oppdatert_modell") %>%
    mutate( across(  .cols = c(kg),  .fns = function(x) factor(x)) ) %>%
    mutate( across(  .cols = c(estimate, str.e),  .fns = function(x)  x*111477)
    ) |>
    rename( did = estimate,
            se = str.e)


# # Graf --------------------------------------------------------------------
# 
# df_did_per_ar %>%
#     #filter( str_detect(kat, "kontro") ) %>%
#     select(ar, kg,did) |>
#     group_by( kg) |>
#     mutate( vekst  = did - lag(did) ) |>
#     summarise( vekst = mean(vekst, na.rm = T))

# # Graf did
# 
ps <- position_dodge( width = 0.7)

graf_did <-
    df_did_per_ar %>%
    mutate( ar = as.numeric(ar)) |>
    #filter( kat == "kontroll") %>%
    # mutate( kg = ifelse(kg == "0.7", " ", "  ")) %>%
    ggplot( aes( y = did, x = ar, fill = kg, color = kg ) ) +
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
    labs( y = "FiF-estimat", x = "" )

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
Sys.setlocale("LC_CTYPE")
graf_pr_ar <- df_bt_per_ar %>%
    mutate( kg = ifelse(kg == "0.8", "Kontrolgruppe: [80%,95%)", "Kontrolgruppe:[90%,95%)")) %>%
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
    labs( y = "Utbetalt barnetillegg" ,x = "År") +
    scale_y_continuous( labels =  function(x) format( x, big.mark = " ") %>% str_c(., " kr"))


graf_utprint <- graf_pr_ar/graf_did
# 
# 
# 
graf_utprint %>% ggsave( filename = "plot/SVG/figurV1.svg", device = "svg", width = 10, height =10 ) 
# 
# 
# 
# 
# 
# 
# 
# 
