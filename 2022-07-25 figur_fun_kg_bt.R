
# Figur skal vise hvordan DID-estimatet endres med KG.

## bibliotek
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
library(patchwork)
windowsFonts(Times=windowsFont("TT Times New Roman"))

# Data
df_bt <- readxl::read_excel("data/2022-05-02 did_fun_KG.xlsx", sheet = 1)

# # Barnetillegg som f(KG), I(dato_num*gr) +I(dato_num*overg_regl) + I(dato_num*gr*overg_regl) 
# df_bt <- readxl::read_excel("data/2022-05-02 did_fun_KG.xlsx", sheet = "bt_ny")

df_inntekt <- readxl::read_excel("data/2022-05-02 did_fun_KG.xlsx", sheet = 2)

openxlsx::getSheetNames("data/2022-05-02 did_fun_KG.xlsx")

df_inntekt <- readxl::read_excel("data/2022-05-02 did_fun_KG.xlsx", sheet = "inntekt") |>  
    mutate(  rowname = str_extract(rowname, "[0-9]\\d.*"))

# Barnetillegget ----------------------------------------------------------

graf_bt <- df_bt %>% 
    mutate( across( .cols = c(estimate, std_error), .fns = function(x) x*106500 )) %>% 
    mutate(  rowname = str_extract(rowname, "[0-9]\\d.*")) |> 
    ggplot( aes( x = kg, y = estimate) ) +
    geom_point() +
    geom_errorbar( aes( ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error)) +
    # geom_errorbar( aes( ymin = estimate - 1.645*std_error, ymax = estimate + 1.645*std_error),
    #                color = "red", widt = .2 ) +
    geom_hline( yintercept = 0, linetype = 2, color = "gray") + 
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr")  ) + 
    facet_wrap(~ rowname) +
    labs( y = NULL, x = "Teoretisk kompensasjonsgrad") +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x  = element_blank()
    )

graf_bt

g <- graf_bt + 
    scale_x_continuous( breaks = c(0.7,0.8,0.9,0,95), labels = function(x) format( str_c(x*100, "%" ))) +
    geom_vline( xintercept = c(0.83), linetype = 2, color = "gray")

#g  |> ggsave( filename = "plot/graf_fun_bt_kg.png", device = "png", width = 6.7, height =4.5 )
#g  |> ggsave( filename = "plot/SVG/figur8.svg", device = "svg", width = 6.7, height =4.5 )




# Inntekt -----------------------------------------------------------------

graf_int <- 
    df_inntekt %>% 
    arrange( rowname) %>% 
    mutate(  rowname = str_extract(rowname, "[0-9]\\d.*")) |> 
    mutate( across( .cols = c(estimate, std_error), .fns = function(x) x*111477*12 ))  |> 
    ggplot( aes( x = kg, y = estimate) ) +
    geom_point() +
    geom_errorbar( aes( ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error)) +
    #geom_errorbar( aes( ymin = estimate - 1.645*std_error, ymax = estimate + 1.645*std_error),
    #              color = "red", width = .05 ) +
    geom_hline( yintercept = 0) +
    facet_wrap(~ rowname) +
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr")  ) + 
    labs( y = NULL, x = "Teoretisk kompensasjonsgrad") +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x  = element_blank()
    )

graf_int <- graf_int + scale_x_continuous( breaks = c(0.7,0.8,0.9,0,95), labels = function(x) format( str_c(x*100, "%" ))) +
    geom_vline( xintercept = c(0.83), linetype = 2, color = "gray")

graf_int |>  ggsave( filename = "plot/graf_fun_inntekt_kg.png", device = "png", width = 6.7, height =4.5 )
graf_int |>  ggsave( filename = "plot/SVG/graf_fun_inntekt_kg.svg", device = "svg", width = 6.7, height =4.5 )



# Inntekt -----------------------------------------------------------------

graf_int <- df_inntekt %>%
    filter( kg < 0.94) |> 
    arrange( rowname) %>% 
    mutate( across( .cols = c(estimate, std_error), .fns = function(x) x*106399*12 )) %>% 
    ggplot( aes( x = kg, y = estimate) ) +
    geom_point() +
    geom_errorbar( aes( ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error)) +
    geom_errorbar( aes( ymin = estimate - 1.645*std_error, ymax = estimate + 1.645*std_error),
                   color = "red", width = .05 ) +
    geom_hline( yintercept = 0) +
    facet_wrap(~ rowname, scales =  "free") +
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") ) + 
    scale_x_continuous( breaks = seq(0.6,0.96, by = .1)) +
    labs( y = NULL, x = "Teoretisk kompensasjonsgrad") +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x  = element_blank()
    )

graf_int + scale_x_continuous(
    breaks = c(0.7, 0.8, 0.9, 0, 95),
    labels = function(x)
        format(str_c(x * 100, "%"))
) +
    geom_vline(xintercept = c(0.83),
               linetype = 2,
               color = "gray")


# 
#graf_int %>% ggsave( filename = "plot/graf_inntekt_fun_kg.png", device = "png",  width = 6.7, height =5 )



