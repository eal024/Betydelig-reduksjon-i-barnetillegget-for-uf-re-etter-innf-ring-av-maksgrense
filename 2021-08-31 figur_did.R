

library(tidyverse)

df_model_did_kunbarn <- readxl::read_excel("data/2021-08-31 data_figur_did.xlsx",sheet = "medbarn")
df_model_did_alle <- readxl::read_excel("data/2021-08-31 data_figur_did.xlsx",sheet = "alle")

# Post tilsvarer gjennomsnittlig inntekt i perioden 2013-2015
data <- tibble( gruppe = c("med barn", "alle"), data = list(df_model_did_kunbarn,df_model_did_alle)) %>% 
    unnest(data) 


# Graf 
graf2_did <- data %>%
    filter( str_detect(term, " berort_2")) %>%
    mutate( gruppe = ifelse( gruppe == "alle", "T = KG > 95%\ncontrollgruppe = Andre uføretrygdede",
                             "T = KG > 95%\nKun de med barn") ) %>% 
    ggplot( aes( x = ar, y = estimate)) + 
    geom_point( size = 3) +
    geom_errorbar( mapping = aes(x =ar,  ymin = (estimate -std.error*1.96), ymax = (estimate + std.error*1.96) ), width = 0.2  ) +
    facet_wrap( ~gruppe , ncol = 1) +
    geom_hline( yintercept = 0, linetype = 2, color = "black") +
    geom_vline( xintercept = 2016, linetype = 2, color = "red" , size = 1., alpha = 0.8) +
    expand_limits( y = c(-0.1, 0.2)
    ) + 
    theme_light() +
    theme( panel.grid.major.x =  element_blank(), 
           panel.grid.minor.x =  element_blank(),
           axis.text  = element_text( size = 12),
           strip.text = element_text( size =12, color = "black")
    ) +
    scale_x_continuous( breaks = c(2012:2019))


ggsave(graf2_did, path = "plot", filename = "graf2_did.png", device = "png", height = 6, width = 12 )


# m <- as_tibble(mtcars) %>% lm( disp  ~cyl, data = .)
# confint( m, level = 0.95)
# 62-5.47*1.96