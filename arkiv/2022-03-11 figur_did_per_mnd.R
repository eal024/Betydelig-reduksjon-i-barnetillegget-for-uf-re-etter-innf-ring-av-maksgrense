

df <- tibble( 
    dato = dato_vec(c(2015:2017))[[2]], 
    len =  1*(1-.01)^(1:length(dato))
    ) %>% 
    expand_grid( gr = c("over", "under")) %>% 
    arrange( gr)

df %>% 
    mutate( )