

# 
library(tidyverse)
library(lubridate)


# 
df <- readxl::read_excel("data/2022-04-04 parallell_trend_test_data.xlsx") |> 
    mutate( behandling = behandling |>  factor( levels = c("[0,95%)", "[95%,98%]","[98%,101%]", "[101%,104%]","[104%,107%]","[107%,110%]","[110%,~)")) %>% fct_rev()) |> 
    filter( behandling != "[0,95%)") |> 
    mutate( dato = ymd(dato))


# Tibble med koordinader for rød linje 
gr <- sort( unique(df$behandling) ) 
d  <- seq(as.Date("2016-01-01"), as.Date("2021-01-01"), by = "year")

rev(d)[1:(length(d)+1-4)]   

test <- list()
test2 <- list()

for(i in 1:length(gr)){
    test[[i]]  <- rep(gr[i], 7-i)
    test2[[i]] <- rev(d)[1:(length(d)+1-i) ]   
    
}

df_rod_linje <- tibble(  behandling   = test |> unlist(),
                         dato = Reduce(c,test2)
                         )



df |> 
    ggplot(aes(x = dato, y = barnetillegg)) +
    geom_line() +
    facet_wrap(~ behandling, scales = "free_y") +
    geom_vline( data = df_rod_linje, aes( xintercept = dato), linetype = 2, color = "red") +
    geom_point( data = df |> filter( dato %in% d) , aes( x =dato, y = barnetillegg), inherit.aes = F, size = 2 )


# Inntekt -----------------------------------------------------------------


df |> 
    filter( year(dato) < 2021) |> 
    ggplot(aes(x = dato, y = inntekt)) +
    geom_line() +
    facet_wrap(~ behandling, scales = "free_y") +
    geom_vline( data = df_rod_linje, aes( xintercept = dato), linetype = 2, color = "red") +
    geom_point( data = df |> filter( dato %in% d, year(dato) < 2021) , aes( x =dato, y = inntekt), inherit.aes = F, size = 2 )




