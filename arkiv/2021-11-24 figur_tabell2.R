


# 
library(tidyverse)
library(extrafont)
library(hrbrthemes)
windowsFonts(Times=windowsFont("TT Times New Roman"))

df <- readxl::read_excel("data/2021-11-24 figur_tabell2.xlsx")

regel_relevant <- c("komp_grad","effekt", "barnetillegg")

df$name %>% unique()
name_relevant  <- c("alder", "antall", "antall_barn", "fodtNorge", "gidt_samboer", "giftTPS", "kvinner", "uføretrygd")

theme_set(theme_ipsum())

df %>% distinct(gr)

graf_tabell2 <-  
    df %>% 
    filter( name %in% name_relevant,
            index %in% c(1,5,4)
            ) %>% 
    mutate( name = case_when(
        name ==  "antall_barn"  ~ "Antall barn",
        name ==  "giftTPS"      ~ "Andel gift (TPS)",
        name ==  "gift_samboer" ~ "Andel registrert\ngift eller samboer",
        name ==  "arbeidstid"   ~ "Arbeidstid",
        name ==  "uforetrygd"   ~ "Utbetalt uføretrygd",
        name == "fodtNorge"     ~ "Fødselsland Norge",
         T ~ str_to_title(name) 
        ) %>%   #) %>% distinct(name)
        factor(
            levels = c(
                "Alder",
                "Antall",
                "Andel gift (TPS)",
                "Andel registrert\ngift eller samboer",
                "Kvinner",
                "Antall barn",
                "Fødselsland Norge",
                "Inntekt",
                "Arbeidstid",
                "Utbetalt uføretrygd"
            )
        )  
    ) %>%
    ggplot( aes( y = value, x = ar, fill= factor(gr), color = factor(gr)) ) +
    geom_point() +
    geom_line( ) +
    facet_wrap( ~ name, scales = "free") +
    theme( legend.position = "none",
           strip.text = element_text( color = "black", size = 10),
           panel.grid.major.x =  element_blank( ),
           panel.grid.minor.x = element_blank( ),
           text = element_text( family = "Times")
    ) +
    scale_color_manual( values = navR::nav_farger()[c(5,2,3)] ) +
    scale_x_continuous( breaks =  c(2015, 2017, 2019,  2021 )) +
    geom_text( aes( label = case_when( (name == "Alder" & gr == "berørte" & ar == 2016) ~ "Berørt",
                                       (name == "Alder" & gr == "ikke berørte, (KG>79%)"  & ar == 2019) ~ "ikke berørte\n(KG>79%)",
                                       (name == "Alder" & gr == "ikke berørte, (KG>89%)" & ar == 2017) ~ "ikke berørte\n(KG>89%)",
                                       T ~"")),
               hjust = +0.3, vjust = +1.1) +
    labs( x = NULL , y = NULL) #+
#    geom_vline( xintercept = 2016, linetype =2, alpha = .5)

df %>% distinct(index, gr)    



# save
graf_tabell2 %>% ggsave( filename = "plot/graf_tabell2.png", device = "png", width = 8, height = 7 )


# -------------------------------------------------------------------------


# Del i 3
df %>% 
    filter(  gr == "berørte", name == "effekt") %>%
    ggplot( aes( y = value, x = ar, fill= factor(gr), color = factor(gr)) ) +
    geom_point() +
    geom_line( ) +
    theme( legend.position = "none" ) +
    scale_y_continuous( labels = scales::format_format( big.mark =" ")) +
    labs( y = "", x = " ")




graf_tabell2_2 %>% ggsave( filename = "plot/graf_tabell2_2.png", device = "png", width = 10, height = 10 )
