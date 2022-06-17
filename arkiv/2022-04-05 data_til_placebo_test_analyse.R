






# Inntekt pr mnd lik figur 5 -----------------------------------------------------------------

library(data.table)
library(lubridate)

# Gjennomsnittlig inntekt, per mnd. og gjennomsnitt per år
dt7 <- readxl::read_excel("data/2022-04-05 figur5_inntekt.xlsx", sheet = 1) |> as.data.table()
dt6 <- readxl::read_excel("data/2022-04-05 figur5_inntekt.xlsx", sheet = 2) |> as.data.table() 

dt6 |> as_tibble()

dt6$behandling

dt6 <- dt6[ , dato := ymd(dato),][ , behandling := factor(behandling, levels = c("[0,95%)", "[95%,98%)","[98%,101%)", "[101%,104%)","[104%,107%)","[107%,110%)","[110%,~)")) %>% fct_rev(),]
dt7 <- dt7[ , behandling := factor(behandling, levels = c("[0,95%)", "[95%,98%)","[98%,101%)", "[101%,104%)","[104%,107%)","[107%,110%)","[110%,~)")) %>% fct_rev(),]

farger <- tibble( behandling = dt7$behandling |> unique() |> sort(), ar = c(2016:2021, 2022 ) )

dt7<- dt7 |> mutate( farger = case_when( behandling == "[110%,~)"    & ar > 2015 ~  1,
                                   behandling == "[107%,110%)" & ar > 2016 ~  1,
                                   behandling == "[104%,107%)" & ar > 2017 ~  1,
                                   behandling == "[101%,104%)"  & ar > 2018 ~  1,
                                   behandling == "[98%,101%)"   & ar > 2019 ~  1,
                                   behandling == "[95%,98%)"   & ar > 2020 ~  1
                                   T~0) 
                     ) |>
    as.data.table() 


#
graf <- 
    dt6[dato < ymd("2021-01-01") & behandling != "[0,95%)" & overg_regl == 1,.(dato, inntekt,behandling),] %>%
        ggplot(aes(y = inntekt, x = dato, fill= behandling, color = behandling )) +
        geom_line() +
        geom_point( data = dt7[ behandling != "[0,95%)" & overg_regl == 1,.(ar, inntekt, behandling, farger),][ , ar := ymd( str_c(ar,"-06-01")), ],
                    aes(y = inntekt, x = ar,
                    color = ifelse( farger == 1, "red", "blue"))
                    ) +
        geom_line( data = dt7[ behandling != "[0,95%)"& overg_regl == 1,.(ar, inntekt, behandling),][ , ar := ymd( str_c(ar,"-06-01")), ], aes(y = inntekt, x = ar ), alpha = 0.1) +
        facet_wrap(~behandling) +
        theme( legend.position = "none") +
        theme_ipsum() +
        theme( legend.position = "none",
               panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               strip.text = element_text( color = "black", size = 10)
        ) +
    labs( x = "År", y = "Inntekt. Punktene viser gjennomsnitt årlig, linje viser snitt mnd")

graf |> ggsave( filename =  str_c("plot/", lubridate::today(), " figur_3_mnd.png"), device = "png" , width = 7.5, height = 6)



# Barnetillegget ----------------------------------------------------------



