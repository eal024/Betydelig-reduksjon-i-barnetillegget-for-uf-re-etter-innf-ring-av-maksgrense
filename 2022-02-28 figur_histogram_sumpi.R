


#
library(tidyverse)
library(hrbrthemes)
library(extrafontdb)
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))


fs::dir_ls("../../../../OneDrive - NAV/Desktop/DataMaxKG/")

# Ønsker ikke å ha dataene tilgjengelig på github
df <- readxl::read_excel("../../../../OneDrive - NAV/Desktop/DataMaxKG/histogram_inntekt.xlsx") %>% 
    mutate( barn = ifelse( barn == 1, "Med barn", "Uten barn"),
            #            sumpi = 106399*sumpi
            index = 1:nrow(df)
    )


int_snitt <- df %>%group_by(barn) %>% summarise( sumpi = mean(sumpi) 
)  

andel <-  df %>% group_by( barn, sumpi == 0) %>% count() 

# Graf histogram
int <- df %>% 
    #filter( sumpi  < 6,  stil_and < 1) %>% 
    #    pivot_longer( -c(ar,barn) ) %>% 
    ggplot( aes( x = sumpi, fill = barn) ) + 
    geom_histogram( aes( y = ..density..), alpha = 0.6, bins = 30) +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    scale_x_continuous( limits = c(0, 5)) +
    scale_y_continuous( limits = c(0, .25)) +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10),
           text = element_text( family = "Times") 
    ) +
    facet_wrap(~barn) +
    labs( x = "Pensjonsgivende inntekt")
int

stilling <- df %>% 
    filter( sumpi  < 7,  stil_and < 1) %>% 
    #    pivot_longer( -c(ar,barn) ) %>% 
    ggplot( aes( x = stil_and, fill = barn) ) + 
    geom_histogram( aes( y = ..density..), alpha = 0.6, bins =30, position = "identity") +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    scale_x_continuous( limits = c(-0.1, 1)) +
    theme_ipsum() +
    theme( legend.position = "none",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           strip.text = element_text( color = "black", size = 10),
           text = element_text( family = "Times") 
    )


library(patchwork)

graf <- int + stilling


int %>%  ggsave( filename =  str_c("plot/", lubridate::today(), " figur_histogram_sumpi.png"), device = "png" , width = 7.5, height = 4)



