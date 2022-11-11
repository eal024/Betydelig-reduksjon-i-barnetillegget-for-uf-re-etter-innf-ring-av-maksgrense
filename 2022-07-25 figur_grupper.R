
library(tidyverse)

# Tidligere
# data <- readxl::read_xlsx("data/2022-07-25 regresjon_subgrupper.xlsx", sheet = 1)

data <- readxl::read_excel("C:/Users/L158017/Google Drive/Artikler/artikkel_95%_regel/2022-03-21 artikkel_barnetillegg.xlsx",
                           sheet = "tabell_vedlegg_bt_og_int")[ c(1:3,5:7)] |> 
    mutate(  across( .cols = c(estimate, std_error), as.numeric)) 

Sys.setlocale("LC_CTYPE")
## 
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


# Grupper. 
Sys.setlocale("LC_CTYPE")

subgrupper <- c("kvinner", "menn", "enslig", "graderte", "1 barn", "2 barn", "3 eller flere barn")

unique(data$utvalg)

graf_bt <- data |> 
    filter( name == "bt", utvalg %in% subgrupper  ) |> 
    ggplot( aes( y = estimate, x = rowname)) +
    geom_point() +
    # geom_point( data = data |>filter( name == "bt", str_detect(utvalg, "samlet effekt")  ) |> select(estimate, rowname) ,
    #             aes( y = estimate, x = rowname),
    #             inherit.aes = F,
    #             color= "red"
    # ) +
    geom_errorbar( aes( ymin = (estimate -std_error*1.96), ymax=  (estimate +std_error*1.96) )) +
    facet_wrap(~ as.factor(utvalg) , nrow = 1) +
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
graf_inntekt <-  
    data |> 
    filter( name != "bt", utvalg %in% subgrupper  ) |> 
    mutate( kategori = "Inntekt") |> 
    ggplot( aes( y = estimate, x = rowname)) +
    geom_point() +
    geom_errorbar( aes( ymin = (estimate -std_error*1.96), ymax=  (estimate +std_error*1.96) )) +
    facet_wrap(~ as.factor(utvalg) , nrow = 1) +
    theme_ipsum_ps() +
    theme( panel.grid.major.x = element_blank(),
               panel.grid.minor.x  = element_blank(),
               axis.text.x = element_text( angle = 90)
    ) + 
    scale_x_continuous( breaks = c(2016:2021) ) + 
    scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") ) +
    labs( x = "", y = "FiF, barnetillegget") +
    geom_hline( yintercept = 0, color ="red", linetype = 2)


graf_inntekt

(graf_bt/graf_inntekt) |>  ggsave( filename = "plot/graf_grupper.png", device = "png",  width = 12, height =8 )
(graf_bt/graf_inntekt) |>  ggsave( filename = "plot/SVG/figur7.svg", device = "svg",  width = 12, height =8 )





# Arkiv -------------------------------------------------------------------

# graf_bt <- data |> 
#     filter( kategori == "bt_") |> 
#     mutate( kategori = "Barnetillegg",
#             rowname = str_remove(rowname, "ar_factort_") |> as.factor(),
#             id = str_remove( id, "bt_") |> str_replace("_", " ") |> str_to_sentence(),
#             across( .cols = c(estimate, std_error), .fns = function(x) x*106399)
#     ) |> 
#     ggplot( aes( y = estimate, x = rowname)) + 
#     geom_errorbar( aes( ymax = estimate + std_error, ymin =estimate - std_error)
#     ) +
#     geom_point() +
#     facet_wrap(~id, nrow =   1) +
#     theme_ipsum_ps() +
#     theme( panel.grid.major.x = element_blank(),
#            panel.grid.minor.x  = element_blank(),
#            axis.text.x = element_blank()
#     ) + 
#     scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") ) +
#     labs( x = "", y = "FiF, barnetillegget") +
#     geom_hline( yintercept = 0, color ="red", linetype = 2)
# 
# # 
# graf_inntekt <-  data |> 
#     filter( kategori != "bt_") |> 
#     mutate( kategori = "Inntekt",
#             rowname = str_remove(rowname, "ar_factort_") |> as.factor(),
#             id = str_remove( id, "bt_") |> str_replace("_", " ") |> str_to_sentence(),
#             across( .cols = c(estimate, std_error), .fns = function(x) x*12*106399)
#     ) |> 
#     ggplot( aes( y = estimate, x = rowname )) + 
#     geom_errorbar( aes( ymax = estimate + std_error, ymin =estimate - std_error)
#     ) +
#     geom_point() +
#     facet_wrap(~id, nrow =   1) +
#     geom_hline( yintercept = 0, linetype = 2, color = "red") +
#     theme_ipsum_ps() +
#     theme( panel.grid.major.x = element_blank(),
#            panel.grid.minor.x  = element_blank(),
#            axis.text.x = element_text( size = 14, angle = 90)
#     ) +
#     #scale_y_continuous( limits = c(-10000,28000)) +
#     labs( x = "", y = "FiF, inntekt") +
#     scale_y_continuous( labels = function(x) str_c(format(x, big.mark  = " "),"kr") )
# 
# graf_inntekt
# 
# (graf_bt/graf_inntekt) |>  ggsave( filename = "plot/graf_grupper.png", device = "png",  width = 12, height =8 )

