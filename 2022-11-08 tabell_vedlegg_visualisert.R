
library(tidyverse)

readxl::excel_sheets("C:/Users/L158017/Google Drive/Artikler/artikkel_95%_regel/2022-03-21 artikkel_barnetillegg.xlsx")

#
data <- readxl::read_excel("C:/Users/L158017/Google Drive/Artikler/artikkel_95%_regel/2022-03-21 artikkel_barnetillegg.xlsx",
                           sheet = "tabell_vedlegg_bt_og_int")[ c(1:3,5:7)] 



data |> 
    filter( name == "bt", !str_detect(utvalg, "samlet effekt")  ) |> 
    ggplot( aes( y = estimate, x = rowname)) +
    geom_point() +
    geom_point( data = data |>filter( name == "bt", str_detect(utvalg, "samlet effekt")  ) |> select(estimate, rowname) ,
                aes( y = estimate, x = rowname),
                inherit.aes = F,
                color= "red"
    ) +
    geom_errorbar( aes(ymin = estimate-1.96*std_error, ymax = estimate+1.96*std_error)) +
    facet_wrap( ~utvalg, scales = "free_y"
    ) +
    geom_hline( yintercept =  0, linetype = 2) +
    theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) )



data |> 
    filter( name == "inntekt", !str_detect(utvalg, "samlet effekt")  ) |> 
    ggplot( aes( y = estimate, x = rowname)) +
    geom_point() +
    geom_point( data = data |>filter( name == "inntekt", str_detect(utvalg, "samlet effekt")  ) |> select(estimate, rowname) ,
                aes( y = estimate, x = rowname),
                inherit.aes = F,
                color= "red"
                ) +
    geom_errorbar( aes(ymin = estimate-1.96*std_error, ymax = estimate+1.96*std_error)) +
    facet_wrap( ~utvalg, scales = "free_y"
                ) +
    geom_hline( yintercept =  0, linetype = 2) +
    theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) )
