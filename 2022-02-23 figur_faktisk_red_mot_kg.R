



# Figur2: Teori KG mot faktisk reduksjon.


library(tidyverse)
library(hrbrthemes)
#windowsFonts(Times=windowsFont("TT Times New Roman"))

dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
}

# Kg, ar og reduksjon
#df <- readxl::read_excel("data/2022-02-23 figurer_tid_kg_effekt.xlsx", sheet = 3)

# Oppdatert data
df <- readxl::read_excel("data/2022-02-23 figurer_tid_kg_effekt.xlsx", sheet = "reduksjon_teori_kg")

graf_13 <- 
    df %>% 
    filter( n > 10, ar %in% c(2016,2018, 2021 ), between(kg, 0.8,  1.15)) |> 
    rename( for_2016 =  overg_regl,
            teoretisk_kom_grad = kg,
            gj_effekt = reduksjon ) |>
    mutate( for_2016 =  ifelse(for_2016 == 1, paste0("Del av overgangsordning"), paste0("Nye mottaker etter 2016")  ) %>% as.factor(),
            gj_effekt = gj_effekt*106399
            ) %>% 
    ggplot( aes(x = teoretisk_kom_grad, y = gj_effekt, fill = as.factor(for_2016), color= as.factor(for_2016)) ) +
    geom_line( size = 1) +
    facet_wrap(~ ar) +
    theme_ipsum(  ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           text = element_text( family = "Times")
    ) +
    scale_fill_manual(values = c(  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c( "#EBB261", "#9D5A6C")) +
    scale_x_continuous( breaks = c(.4, .7, .95 , 1.15),
                        labels = function(x) ifelse( x == 0.95, format(x, digits =2) %>% dropLeadingZero, format(x, digits =1) %>% dropLeadingZero ) ) +
    labs( y = "Reduksjon fra 2016-reglen (2021G)", x = "Kompensasjonsgrad før avkorting") +
    scale_y_continuous( labels = function(x) paste(format(x, big.mark = " "), " kr"), limits = c(0,50000)) +
    geom_vline( xintercept = 0.93, linetype =2, color= "red")


graf_13 %>% ggsave( filename = "plot/graf_13.png", device = "png", width = 8, height =5 )
