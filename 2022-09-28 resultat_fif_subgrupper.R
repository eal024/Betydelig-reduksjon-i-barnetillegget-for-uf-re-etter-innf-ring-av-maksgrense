
library(tidyverse)
library(hrbrthemes)
Sys.setlocale("LC_CTYPE")
# getworksheet
file_dir <- "data/2022-07-25 regresjon_subgrupper.xlsx"

openxlsx::getSheetNames(file_dir)

data <- readxl::read_xlsx("data/2022-07-25 regresjon_subgrupper.xlsx", sheet = "andreutkast")



# Grupper. 
df_barnetillegg <- data |> 
    rename( id = name) |> 
    filter( id == "bt", !str_detect(utvalg, "graderte, menn") ) |> 
    fill(pri) |> 
    mutate( kategori = "Barnetillegg",
            rowname = str_remove(rowname, "ar_factort_") |> as.factor(),
            id = str_remove( id, "bt_") |> str_replace("_", " ") |> str_to_sentence(),
            across( .cols = c(estimate, std_error), .fns = function(x) x*111477),
            utvalg = fct_reorder(utvalg, pri)
            
    )

graf_bt <- df_barnetillegg |>  
    ggplot( aes( y = estimate, x = rowname)) + 
    geom_errorbar( aes( ymax = estimate + 1.96*std_error, ymin =estimate - 1.96*std_error)
    ) +
    geom_point() +
    facet_wrap(~utvalg, nrow =   1) +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x  = element_blank(),
           axis.text.x = element_blank()
    ) + 
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") ) +
    labs( x = "", y = "FiF, barnetillegget") +
    geom_hline( yintercept = 0, color ="red", linetype = 2)


graf_bt
# 

df_inntekt <-  
    data |> 
    rename( id = name ) |> 
    fill(pri) |> 
    filter( id != "bt", !str_detect(utvalg, "graderte, menn, under") )|> 
    mutate( kategori = "Inntekt",
            rowname = str_remove(rowname, "ar_factort_") |> as.factor(),
            id = str_remove( id, "bt_") |> str_replace("_", " ") |> str_to_sentence(),
            across( .cols = c(estimate, std_error), .fns = function(x) x*12*111477),
            utvalg = fct_reorder(utvalg, pri)
    ) 
    
graf_inntekt <- df_inntekt |>     
    ggplot( aes( y = estimate, x = rowname )) + 
    # geom_errorbar( aes( ymax = estimate + 1.96*std_error, ymin =estimate - 1.96*std_error)
    # ) +
    geom_errorbar(
        #data = df_inntekt |> filter( str_detect(utvalg, "graderte")),
        aes( x = rowname, 
             y = estimate, 
             ymax = estimate + 1.96*std_error, ymin =(estimate - 1.96*std_error)) #,
        #inherit.aes = F,
        #color = "red",
        #alpha = 0.5,
        #size = .9
    ) +
    geom_point() +
    facet_wrap(~utvalg, nrow =   1) +
    geom_hline( yintercept = 0, linetype = 2, color = "red") +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
           panel.grid.minor.x  = element_blank(),
           axis.text.x = element_text( size = 14, angle = 90) #,
           #strip.text.x = element_text(size = 6)
    ) +
    #scale_y_continuous( limits = c(-10000,28000)) +
    labs( x = "", y = "FiF, inntekt") +
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") )

graf_inntekt

library(patchwork)
graf_inntekt

#(graf_bt/graf_inntekt) |>  ggsave( filename = "plot/graf_grupper.png", device = "png",  width = 12, height =8 )
(graf_bt/graf_inntekt) |>  ggsave( filename = "plot/SVG/graf_grupper.svg", device = "svg",  width = 13, height =8 )

#
Sys.setlocale("LC_CTYPE")
df_inntekt |> 
    mutate( 
        signif = case_when( pr_t < 0.01  ~ "**",
                            between(pr_t, 0.0011,0.1001) ~ "*",    
                            between(pr_t, 0.049,0.1001)  ~ ".",
                            pr_t < 0.101 ~ ".",
                            T ~ " "),
        verdi = paste0( format(estimate, digits = 1, big.mark = " ") , signif)
    ) |> 
    select(id, utvalg, rowname, verdi) |> 
    pivot_wider( names_from = rowname, values_from = verdi)

df_barnetillegg |> filter(str_detect(utvalg, "grader"))

df_inntekt |> 
    select(id, utvalg, rowname, verdi = estimate) |> 
    pivot_wider( names_from = rowname, values_from = verdi) |> 
    bind_rows(
        df_barnetillegg |> 
            select(id, utvalg, rowname, verdi = estimate) |> 
            pivot_wider( names_from = rowname, values_from = verdi)
        
    ) |> 
    writexl::write_xlsx("data_export/fif_grupper.xlsx")
    
