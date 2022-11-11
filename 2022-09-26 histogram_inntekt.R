

#
library(tidyverse)


# 
df <- readxl::read_excel("data/2022-09-23 inntekt_col.xlsx")


# Graf
graf_histogram_inntekt_21 <- df |> 
    ggplot( aes( y = andel, x = inntekt_c*111477, color = gr, linetype = gr)) +
    geom_point( ) +
    geom_line( alpha = 0.8, size = 1) +
    scale_fill_manual(values = c( "#ff9100", "green4")) +
    scale_colour_manual(values = c( "#ff9100", "green4")) +
    scale_x_continuous( 
                        labels = function(x) paste0(
                            format(x, big.mark = " "), " kr"
                            ),
                        limits = c(0, 70000)
                        ) +
    #annotate( geom = "text", y = 0.72, x = 7000, label = ">95%", colour = "#ff9100") +
    #annotate( geom = "text", y = 0.2, x = 21000, label = "[85%,95%)", colour = "green4") +
    theme_ipsum(  ) +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 14),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           axis.text.x = element_text( size = 14),
           text = element_text( family = "Times")
    ) +
    labs( x = "Inntekt, A-ordningen ", y = "Andel", caption = "Kilde:NAV")


graf_histogram_inntekt_21 |> ggsave( filename = "plot/SVG/graf_fordeling_inntekt.svg", device = "svg", width = 8, height =4 )


