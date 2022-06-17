
library(tidyverse)
library(tidyverse)
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_light())

df_figur1 <- readxl::read_excel("data/2021-11-04 figur_data.xlsx", sheet = 1)

# Data figur tabell 2
df <- readxl::read_excel("data/2021-11-24 figur_tabell2.xlsx")


## Figur 1: Kompensasjonsgrad mot inntekt før uførhet
figur_1 <- df_figur1 %>% 
    ggplot( aes( y = kg, x = ar, fill = factor(tt), color = factor(tt) ) )  +
    geom_line( size = 0.9, alpha = 0.9) +
    geom_point( alpha = 0.5) +
    geom_hline( yintercept = 0.95, linetype = 2, color = "red", size = 1) +
    expand_limits( y = c(0.5,1) ) +
    theme( legend.position =  "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank()
           ) +
    scale_y_continuous( labels = scales::percent_format()) +
    labs( y = "Kompensasjonsgrad", x = "") +
    scale_color_manual(values=c("green4", "blue4")) +
    annotate( geom = "text", x = 2019, y = 0.9, label = "Berørte") +
    annotate( geom = "text", x = 2018, y = 0.66, label = "Ikke berørte\nuføretrygdede med barn")



figur_1 %>% ggsave( height = 3.5, width = 4.5, filename = "plot/figur1.png", device = "png" )   

# Figur 3 -----------------------------------------------------------------

df %>%
    filter( gr %in% c("berørte", "uft m. barn, ikke berørt"), name == "komp_grad") %>% 
    ggplot( aes(x = ar, y = value , fill= gr, color =gr) ) +
    geom_point( size = 1, color = "black") +
    geom_line( size =1) +
    theme( legend.position =  "none",
           panel.grid.major.x =  element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text  = element_text( size  =10)
           ) +
    annotate( geom = "text", x = 2019, y = 0.9, label = "Berørte"                              ,family = "Times", color = navR::nav_farger()[5] , size = 4.5) +
    annotate( geom = "text", x = 2018, y = 0.7, label = "Uføretrygdede med barn\nog ikke berørte\n",family = "Times" , color = navR::nav_farger()[1], size = 4.5) +
    scale_y_continuous( labels = function(x) glue::glue( "{format(x*100,digits = 2) }%") , limits = c(0.3, 1.1) ) +
    labs( y = "kompensasjonsgrad", x = "")  +
    scale_color_manual( values = c(navR::nav_farger()[5],navR::nav_farger()[1])) -> figur3

# Save the figure
figur3 %>% ggsave( filename =  str_c("plot/", lubridate::today(), " figur3.png"), device = "png" , width = 5.5, height = 3.5)

# Figur 4 -----------------------------------------------------------------

df %>%
    filter( gr %in% c("berørte", "uft m. barn, ikke berørt"), name == "effekt") %>% 
    mutate( value = ifelse( str_detect(gr, "ikke"), 0, value) ) %>% 
    ggplot( aes(x = ar, y = value , fill= gr, color =gr) ) +
    geom_point( size = 1, color = "black") +
    geom_line( size =1) +
    theme( legend.position =  "none",
           panel.grid.major.x =  element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text  = element_text( size  =10)
    ) +
    annotate( geom = "text", x = 2019, y = 50000, label = "Berørte"                              ,family = "Times", color = navR::nav_farger()[5] , size = 4.5) +
    annotate( geom = "text", x = 2018, y = 1000, label = "Uføretrygdede med barn\nog ikke berørte\n",family = "Times" , color = navR::nav_farger()[1], size = 4.5) +
    scale_y_continuous( labels =  function(x) str_c( format(x, digits = 0, big.mark = " "), "kr") ) +
    labs( y = "Fratrekk i kroner (om 95%-taket opphører)", x = "")  +
    scale_color_manual( values = c(navR::nav_farger()[5],navR::nav_farger()[1])) -> figur4 

# Save the figure
figur4 %>% ggsave( filename =  str_c("plot/", lubridate::today(), " figur4.png"), device = "png" , width = 5, height = 3.5)


# Barnetillegget ----------------------------------------------------------


df %>%
    filter( gr %in% c("berørte", "uft m. barn, ikke berørt"), name == "barnetillegg") %>% 
    ggplot( aes(x = ar, y = value , fill= gr, color =gr) ) +
    geom_point( size = 1, color = "black") +
    geom_line( size =1) +
    theme( legend.position =  "none",
           panel.grid.major.x =  element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text  = element_text( size  =10)
    ) +
    annotate( geom = "text", x = 2019, y = 0.8, label = "Berørte"                              ,family = "Times", color = navR::nav_farger()[5] , size = 4.5) +
    annotate( geom = "text", x = 2018, y = 0.2, label = "Uføretrygdede med barn\nog ikke berørte\n",family = "Times" , color = navR::nav_farger()[1], size = 4.5) +
    scale_y_continuous( labels =  function(x) str_c( format(x, digits = 2, big.mark = " "), "G") ) +
    labs( y = "observert utbetalt barnetillegg", x = "")  +
    scale_color_manual( values = c(navR::nav_farger()[5],navR::nav_farger()[1])) -> figur5 

# Save the figure
figur5 %>% ggsave( filename =  str_c("plot/", lubridate::today(), " figur5.png"), device = "png" , width = 6, height = 4)




# Figur2 ------------------------------------------------------------------

df_figur2 <- readxl::read_excel("data/2021-11-04 figur_data.xlsx", sheet = 2)

df_figur2 %>% 
        filter( ar %in% c(2015,2020)) %>% 
        ggplot( aes( y= kg, x = ifuo_belop_opj, fill = factor(ar), color = factor(ar) )) +
        geom_line( size =1) +
        # geom_smooth( se = T, alpha = 0.1) +
        geom_hline( yintercept = 0.95 , linetype = 2, color = "red", size = 1) +
        expand_limits( x = c(3,10), y = c(0.4, 1) ) +
        scale_color_manual(values=c("green4", "blue4")) +
    annotate( geom = "text", x = 3.5, y = 0.78, label = "2020", color = "blue4", size = 4.5) +
    annotate( geom = "text", x = 4, y = 0.9, label = "2015", color = "green4", size = 4.5) +
    labs( y = "Kompensasjonsgrad (samtlige uføre med barn)", x =  "Inntekt før uførhet (i G)") +
    scale_y_continuous( labels = scales::percent_format( accuracy =1),
                        breaks = seq(from = 0.3, to = 1.05, by = 0.1)) +
    theme( legend.position =  "none" , text = element_text( family =  "Times"  ),
           axis.title = element_text( size = 12),
          # panel.grid.minor.x = element_blank()
           )  -> figur_2
    

figur_2 %>% ggsave( height = 4., width = 5.5, filename = "plot/figur2.png", device = "png" )    








