





# Data og pakker ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(sandwich)
library(lmtest)

# Source
#source()

# Tilrette legge data. Forberedelse til denne ligger i "2022-02-10 husholdninger_samlet.R"
df_hus <- 
    # Fra husholdning samlet
    df_samlet %>% 
    select(-inntekt_g) %>% 
    # Beholder kun mottakere med EPS
    filter( ant_barn  > 0, !is.na(fk_person1_eps)) %>% 
    # Kobler til innktet per år.
    left_join( inntekt_pr_ar, by = c("ar", "fk_person1")) %>% 
    # Ekeltefelle
    left_join( inntekt_pr_ar %>% rename(fk_person1_eps = fk_person1 ), by = c("ar", "fk_person1_eps"), suffix  = c("", "_eps") ) %>% 
    # Forenkler variabel-utvalget
    select( ufor_flagg, ar, fk_person1, fk_person1_eps, teoretisk_kom_grad, inntekt_g, inntekt_g_eps, ant_barn, fodel, NOR,   kjonn, alder) %>% 
    mutate(
        gr = ifelse(teoretisk_kom_grad > 0.9499,1,0),
        konvertet = ifelse( fk_person1 %in% vec_converted, 1, 0 ),
        overg_rlg = ifelse( fk_person1 %in% vec_transition, 1, 0 ),
        post = ifelse( ar > 2015, 1, 0),
        nor = NOR
    ) %>% 
    replace_na( list(inntekt_g = 0, inntekt_g_eps = 0)) %>% 
    mutate( inntekt_g_hus = inntekt_g + inntekt_g_eps)



# Modellene ---------------------------------------------------------------

model_did_hus <- function( df, start, slutt, model, kun_barn = 1) {
    ## Set model
    # model1 uten kontrollvariabler
    if(model == 1)       { model = str_c("inntekt_g_hus ~ post + gr + I(gr*post)")
    # Model 2: mellomset
    } else if( model == 2){ model = str_c("inntekt_g_hus  ~  gr + post + I(gr*post) + ant_barn + kjonn + kjonn + konvertet + overg_rlg")
    # Model 3: Alt av kontrollvariabler
    } else{                 model = str_c("inntekt_g_hus  ~  gr + post + I(gr*post) + ant_barn + kjonn + alder + nor +   konvertet + overg_rlg")
    }
    
    # To data.table
    dt <- as.data.table(df)
    
    # Transformation  
    # Filtering only kids and year start and end.
    dt1 <- dt[ ant_barn >= kun_barn & ar %in% c(start,slutt), ,][,
                                                                 # Child to factor
                                                                 ant_barn := as.character(ant_barn) ,][ ,ant_barn := fct_lump(as.factor(ant_barn), n = 4, other_level = ">4"), ]
    # return df_1
    # df_1 <- as_tibble(dt1)
    # Model to sim
    model <- lm( formula = model,  data = dt1)
    # Return
    return(model)
}

## Returning robust tibble
robust_tibble_hus <- function( tbl_models ){
    tbl_models %>% 
        select(-data) %>% 
        # s = year end (slutt)
        pivot_longer(-s, names_to = "key", values_to = "model") %>% 
        mutate( robust = map(model, ~.x %>% vcovHC(type = "HC1") ),
                robust = map(robust, ~sqrt(diag(.x)) )
        ) %>%
        # Keep only key and robust
        select(-model) %>% 
        pivot_wider(  names_from = key, values_from = robust)
    
}

# Modelltesting  --------------------------------------------------------------------

model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2016, kun_barn = 1) %>% summary()
model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2017, kun_barn = 1) %>% summary()
model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2018, kun_barn = 1) %>% summary()
model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2019, kun_barn = 1) %>% summary()
model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2020, kun_barn = 1) %>% summary()
model_did_hus( model = 3, df = df_hus %>% filter( teoretisk_kom_grad > 0.70), start = 2015, slutt = 2021, kun_barn = 1) %>% summary()


#   Modelleringsfunksjoner --------------------------------------------------------------

tbl_models_function_hus  <- function( teoretisk_kg ){
    
    # Bruker DID-formula for hvert år 2016-2021 
    tbl_models <- tibble( slutt = c(2016:2021),  data = list(
        # Data: Husholdningsdata
        df_hus 
        # Her kommer input -> Filter KG høyere enn bestemt
        %>% filter( teoretisk_kom_grad > teoretisk_kg) ) ) %>%
        mutate( 
            # Model 1 - ingen kontroll
            model1_kun_barn = map2(data,slutt ,function(x,s) model_did_hus(model = 1, df = x, start = 2015, slutt = s, kun_barn = 1 ) ),
            # Model 2 - kontroll
            model3_kun_barn = map2(data,slutt ,function(x,s) model_did_hus(model = 3, df = x, start = 2015, slutt = s, kun_barn = 1 ) )
        )
    
    # Returerer nested tibble
    tbl_models
}





# Resultat ----------------------------------------------------------------

#
modellene_hus <- tbl_models_function_hus( teoretisk_kg = 0.80)
tbl_robust_matrix_hus <- robust_tibble( tbl_models = modellene_hus   )

# Models: se på resultatet
stargazer::stargazer(modellene_hus$model1_kun_barn  ,se = tbl_robust_matrix_hus$model1_kun_barn       , type  ="text")
stargazer::stargazer(modellene_hus$model3_kun_barn  ,se = tbl_robust_matrix_hus$model3_kun_barn       , type  ="text")


# Data til DID-graf ----------------------------------------------------------------

# Hjelpefunksjon til å ta ut tibble med data
create_tbl_did_hus <- function( liste_modeller){
    #
    tibble(ar  = 2016:2021,
           did = map_dbl( map( liste_modeller, function(x) coef(summary(x))[,1]), 4),
           se  = bind_rows( robust_tibble( tibble( slutt = 2016:2021, model = liste_modeller, data = 1) )  %>% pull( model))%>% select(  model_se = dplyr::matches(" * post") ) %>% pull(model_se)
    )
}


# Model 70%
modellene_hus <- tbl_models_function_hus( teoretisk_kg = 0.7)

#  Både med kontroll og ikke-kontroll
df_did <- bind_rows( 
    # Bruk av hjelpefunksjon
    create_tbl_did(liste_modeller = modellene_hus$model1_kun_barn) %>% mutate( kat = "uten kontroll"),
    create_tbl_did(liste_modeller = modellene_hus$model3_kun_barn) %>% mutate( kat = "kontroll")
) %>%
    # For å gi id
    mutate( kg = 0.7)

# Model 80%

modell_2_hus <- tbl_models_function_hus( teoretisk_kg = 0.8)

# Lager Tibble
df_did2 <- bind_rows( 
    # Enkel
    create_tbl_did(liste_modeller = modell_2_hus$model1_kun_barn) %>% mutate( kat = "enkel"),
    # Med kontroll
    create_tbl_did(liste_modeller = modell_2_hus$model3_kun_barn) %>% mutate( kat = "kontroll")
) %>% 
    # Med id
    mutate( kg = 0.8)

# Utprint av samlet data
bind_rows( df_did,df_did2 ) %>% writexl::write_xlsx( str_c("data_export/", lubridate::today(), " did_figur_inntekt_70_og_80.xlsx"))

# # Did-data ut-print
# # Graf 1: Skal passe graf 1
# ps <- position_dodge( width = 0.7)
# 
# # 
# bind_rows( df_did,df_did2 ) %>% 
#     filter( str_detect(kat, "uten") | str_detect(kat, "enkel") ) %>% 
#     ggplot( aes( y = did, x = ar, fill = factor(kat), color = factor(kat))) +
#     geom_errorbar( aes(ymin = did - se*1.6, ymax = did+se*1.6), position = ps) +
#     facet_wrap(~kg) +
#     geom_hline( yintercept = 0, color ="red", linetype = 2) +
#     theme_minimal() +
#     geom_point( position = ps) +
#     theme( legend.position = "none")



# Data til geom_line() ----------------------------------------------------

# Inndeling i grupper
gr_kg <- c("[95%,~)", "[80,95)", "[70,95)", "[0,95)") 

# Faktisk barnetillegg per år, fordelt etter ulike grupper
df_inntekt <- tibble(
    fra    = c(0.95,.80, .70, 0),
    til    = c(4, rep(0.9499, times = 3) ),
    gr     = map2_chr( fra, til, function(x,y)  { paste0( "[",x, ",", if(y == 4){paste0("~)")}else{ paste0( format(y,digits = 2) ,")")} ) } ),
    data   = list(data_9)
) %>% 
    mutate( data = pmap(list(fra = fra, til = til, data = data), function(til, fra, data){ 
        # Filter data
        data[ between(teoretisk_kom_grad, fra ,til) & ant_barn > 0,
              # Her sum g_inntekt      
              , ][ , .( inntekt = mean(inntekt_g,na.rm = T))
                   , by = c("ar")]  %>% as_tibble() }) 
    ) %>% 
    select(gr, data) %>% 
    unnest( cols = c(data)) %>% 
    arrange( gr, ar)   

# Klar-gjør data til grafisk utpritn
df_inntekt_per_ar <- bind_rows(
    # kontrollgruppe 80%
    df_inntekt %>% filter( gr %in% c("[0.8,0.95)","[0.95,~)")) %>% mutate( kg = 0.8),
    # Kontrollgruppe 70%
    df_inntekt %>% filter( gr %in% c("[0.7,0.95)","[0.95,~)")) %>% mutate( kg = 0.7)
) 

df_inntekt_per_ar %>% writexl::write_xlsx( str_c("data_export/", lubridate::today(), "inntekt_per_ar.xlsx"))




