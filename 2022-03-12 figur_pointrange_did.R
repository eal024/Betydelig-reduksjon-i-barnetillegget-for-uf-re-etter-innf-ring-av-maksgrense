



library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
library(extrafont)
#extrafont::font_import()
windowsFonts(Times=windowsFont("TT Times New Roman"))

# data
df <- readxl::read_excel("data/2022-03-12 data_pointrange.xlsx") %>%
    mutate( dato = paste0( str_sub(periode, 1,4), "-", str_sub(periode, 5,6), "-01") %>% ymd() 
            )  

# 
df1 <- df %>%
    mutate( keep = ifelse( (  str_detect(kategori, "nye etter") & dato < ymd("2016-02-01")) , F, T ),
            kategori = fct_rev(kategori)
    ) %>%
    rename( barntilegg = snitt) %>% 
    filter( keep == T,
            month(dato) %in% c(1,3,5,7,9,11)  
    ) 



# Figur før 2016 ----------------------------------------------------------

df2 <- df1 %>% filter( kategori %in% c("overgangordning, 12 mnd") ) 

df2 %>% distinct(gr)

a <- df2 %>% 
    ggplot( aes( y = barntilegg, x = dato ,
                 color = factor(gr) 
                 ) ) +
    geom_linerange( aes( ymin = (barntilegg -sd*ifelse(gr == "[0.948,~)", 1.96, 2.1) ), ymax = (barntilegg +sd*ifelse(gr == "[0.948,~)", 1.96, 2.1))) , linetype = 1) +
    geom_point(    color = ifelse( str_detect(df2$periode, "01$"), "red", "black") , alpha = 0.5, size =2 ) +
    #goem_line( aes(y = barnetilegg, x = dato), group = 1) %>% 
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 14),
           text = element_text( family = "Times") ) +
    scale_color_manual( values = c(navR::nav_farger()[5],navR::nav_farger()[1],navR::nav_farger()[4],navR::nav_farger()[2] )  ) +
    labs( y = "Faktisk utbetalt barnetillegg", x = "År") +
    lims( x = c(ymd("2015-01-01", ymd("2021-12-01"))),  y = c(0.0, 1))
    

a

# Figur mottakere ny etter 2016 -------------------------------------------

df3 <- df1 %>% filter( kategori %in% c("nye etter 2016, alle") , dato > ymd("2016-07-01"))

b <- df3 %>%  ggplot(aes( y = barntilegg, x = dato ,
                #color = factor(gr) 
    ) ) +
    geom_point( color = ifelse( str_detect(df3$periode, "01$"), "red", "black") , alpha = 0.5, size =2) +
    geom_linerange( aes( ymin = (barntilegg -sd), ymax = (barntilegg +sd)) , linetype = 1) +
    #geom_point(color = ifelse( str_detect(df1$periode, "01$"), "red", "black"), alpha = .5, shape = 1 ) +
    #goem_line( aes(y = barnetilegg, x = dato), group = 1) %>% 
    theme_ipsum() +
    theme( legend.position = "none",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text( color = "black", size = 10),
            text = element_text( family = "Times")
        ) +
    lims( x = c(ymd("2015-01-01", ymd("2021-12-01"))),  y = c(0.0, 0.8))

b

library(patchwork)

a+b
    
    
 