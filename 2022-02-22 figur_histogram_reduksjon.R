

library(tidyverse)
library(ggthemes)  # for a mapping theme
library(ggalt)  # for custom map projections
library(ggrepel)  # for annotations
library(viridis)  # for nice colours
library(PupillometryR)
source("~/R-prosjekter/maxKG/appendix.R")

dropLeadingZero <- function(l){
    str_replace(l, '0(?=.)', '')
}

# Se et enkelt eksempel helt nederst.


# Histogramet1 -------------------------------------------------------------

# Avkortingsfordeling over år.

#data <- readxl::read_excel("data/2022-02-22 histogram.xlsx", sheet = 1)

# Data
data2 <- readxl::read_excel("data/2022-02-22 histogram.xlsx", sheet = 2) %>% 
    filter(ar %in% c(2016,2017,2018, 2019, 2020 , 2021)) %>% 
    mutate( ar = as.factor(ar),
            effekt = effekt*106399 )

graf_hist1 <- 
    ggplot(data = data2, aes(y = effekt, x = ar , fill = ar )) +
    geom_boxplot(aes(colour = ar  ), width = 0.2) +
    # The half violins
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    geom_point(aes(y = effekt, color = ar), 
               position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
    # The boxplots
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) 

graf_hist1

graf_hist1_2 <- graf_hist1 + 
    # \n adds a new line which creates some space between the axis and axis title
    labs( y = NULL, x = NULL) +
    scale_fill_manual(values = c("#5A4A6F", "#E47250", "#EBB261",  "#EBB262", "#9D5A6C","#9D5A7C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250", "#EBB261",  "#EBB262", "#9D5A6C","#9D5A7C")) +
    theme_niwot() +
    guides(fill = FALSE, color = FALSE) +
    scale_y_continuous( breaks = seq( from = 0, to = 350000, by = 50000), labels = function(x) str_c( format(x, big.mark = " ", digits = 0)) , limits = c(0,200000)) +
    coord_flip( )

graf_hist1_2

#graf_hist1_2 %>% ggsave( filename = "plot/graf_hist1.png", device = "png", width = 7.5, height = 5 )

# Histogram 2 -------------------------------------------------------------

# Avkorting etter antall barn.

graf_hist2 <- data2 %>%
    filter( 
        #ar %in% c(2016,2021),
        #ant_barn < 5,    
        ar %in% c(2021)
    ) %>% 
    # Omgjøre antall barn til faktor.
    mutate( 
        ant_barn = ifelse( ant_barn > 4, "4<", ant_barn ) %>% factor(),
        ant_barn = factor( ant_barn , levels = c("1", "2", "3", "4", "4<"))
        ) %>%
    group_by(ant_barn) %>% 
    ggplot(aes(y = effekt, x = ant_barn , fill = ant_barn )) +
    geom_boxplot(aes(colour = ant_barn  ), width = 0.2) +
    # The half violins
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    geom_point(aes(y = effekt, color = ant_barn), 
               position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
    # The boxplots
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    # \n adds a new line which creates some space between the axis and axis title
    labs( y = NULL, x = "Antall barn") +
    scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C", "#9D5A7C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C", "#9D5A7C")) +
    theme_niwot() +
    guides(fill = FALSE, color = FALSE) +
    coord_flip() +
    scale_y_continuous( breaks = seq(0,150000, by =50000), labels = function(x)format(x, big.mark = " ", digits = 0), limits = c(0,150000)) 


# For utprint.
library(patchwork)

g <- graf_hist1_2 + graf_hist2


g %>% ggsave( filename = "plot/graf_histogram_effekt.png", device = "png", width = 10, height = 5 )


        
# data <- tibble( ar = 2015:2018) %>% 
#     expand_grid( effekt = rnorm(n = 100, mean = 0.5, sd = 0.1 ) )
# 
# 
# data %>% 
#     mutate( ar = as.factor(ar)) %>% 
#     ggplot(aes(y = effekt, x = ar , fill = ar )) +
#     geom_boxplot(aes(colour = ar  ), width = 0.2) +
#     # The half violins
#     geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
#     geom_point(aes(y = effekt, color = ar), 
#                position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
#     # The boxplots
#     geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
#     # \n adds a new line which creates some space between the axis and axis title
#     labs(y = "Tekst\n", x = NULL) +
#     scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
#     scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
#     theme_niwot() +
#     guides(fill = FALSE, color = FALSE) +
#     coord_flip() 
# 

