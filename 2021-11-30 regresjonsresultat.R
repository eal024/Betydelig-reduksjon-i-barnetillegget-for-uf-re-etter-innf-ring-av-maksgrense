


library(tidyverse)
theme_set(theme_light())


formula <- y ~ x + I(x^2)

figur <- function(df, tekst){
    df %>% filter(str_detect(term, "post ")) %>% 
        ggplot( aes( x = ar, y = estimate)) +
        geom_errorbar( aes( ymin =  (estimate - std.error*1.96),
                            ymax =  (estimate + std.error*1.96)
        ),
        width = 0.2,
        size = 0.1
        ) +
        geom_point( size = 3) +
        geom_hline( yintercept =  0, linetype = 2, color = "red")
}

df_figur <- tibble( sheet = c("med_kontroll", "uten_kontroll"),
                    figurTekst = c("'model:' * y== alpha + beta (post%*%g)  + gamma%*%X + epsilon",
                                   "'model:' * y== alpha + beta (post%*%g) + epsilon"
                                   )
                    ) %>% 
    mutate( data = map(sheet, function(x) readxl::read_excel("data/2021-11-30 regresjons_resultat.xlsx", sheet = x) ),
            graf = map(data , function(x) figur(x) ),
            graf = map2( graf, figurTekst, function(x,y) x + annotate("text", x= 2017, y = 0.25, parse =T,
                                                                      size =4,
                                                                      label = y
                                                                      ))
            )


df_figur$graf[[1]] %>% ggsave( filename = "plot/")

library(patchwork)

a <- df_figur$graf[[1]]; b <- df_figur$graf[[2]]

(a+b) %>% ggsave(filename = "plot/regresjonsresultat.png", device = "png", width = 9, height = 5)

df_figur$data[[2]] %>% filter( str_detect(term, "post "))

df_figur$graf[[1]] + 
    annotate("text", x = 2017.5, y = 0.25, parse = TRUE, size = 4,
                             label = "'model:' * y== alpha + beta (post%*%g)  + gamma%*%X + epsilon") +
    labs( y = "Beta", x = "") +
    lims( y = c(-0.2, 0.25) )

df_figur$graf[[2]] + labs( y = "Estimert mer-inntekt (sumpi) i grunbeløp", x = "") +
    annotate("text", x = 2017., y = 0.25, parse = TRUE, size = 4,
             label = "'model:' * y== alpha + beta (post%*%g) + epsilon") +
    labs( y = "Beta", x = "") + lims( y = c(-0.2, 0.25) )
