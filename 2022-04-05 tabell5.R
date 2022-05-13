


# Tabell 5: Placebo-test

# # tabell 5 --------------------------------------------------------------

# Inntekt
df <- readxl::read_excel("data/2022-04-05 tabell5.xlsx", sheet = "inntekt")

# Tabell inntekt
inntekt <- bind_rows( 
        df |>  select(ar,ovre,coef) |> mutate(coef = coef*106399) |> pivot_wider( names_from = ovre, values_from = coef),
        df |>  select(ar,ovre,se = st.error) |> mutate(se = se*106399) |> pivot_wider( names_from = ovre, values_from = se),
        df |>  select(ar,ovre,t) |> pivot_wider( names_from = ovre, values_from = t)
              ) |> 
    arrange(ar) |> 
    mutate( type = rep(c("estimat", "se", "t"), times = length(2016:2020) )
            ) |> 
    relocate( type , .before = ar) |> 
    pivot_longer( -c(type, ar)) |> 
    pivot_wider( values_from = value, names_from = ar) |> 
    arrange( desc(name) ) |> 
    mutate( name = str_c("95%-", as.numeric(name)*100, "%") ) 

# Barnetillegget
df <- readxl::read_excel("data/2022-04-05 tabell5.xlsx", sheet = "barnetillegg")


barnetillegget <- bind_rows( 
    df |>  select(ar,ovre,coef) |> mutate(coef = coef*106399) |> pivot_wider( names_from = ovre, values_from = coef),
    df |>  select(ar,ovre,se = st.error) |> mutate(se = se*106399) |> pivot_wider( names_from = ovre, values_from = se),
    df |>  select(ar,ovre,t) |> pivot_wider( names_from = ovre, values_from = t)
) |> 
    arrange(ar) |> 
    mutate( type = rep(c("estimat", "se", "t"), times = length(2016:2020) )
    ) |> 
    relocate( type , .before = ar) |> 
    pivot_longer( -c(type, ar)) |> 
    pivot_wider( values_from = value, names_from = ar) |> 
    arrange( desc(name) ) |> 
    mutate( name = str_c("95%-", as.numeric(name)*100, "%") ) 



#
list( inntekt = inntekt, barnetillegget = barnetillegget) |> writexl::write_xlsx("data_export/tabell5_utprint.xlsx")
















