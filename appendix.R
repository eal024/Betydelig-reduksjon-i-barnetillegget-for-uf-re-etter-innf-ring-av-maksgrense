


#
library(extrafont)
windowsFonts(Times=windowsFont("TT Times New Roman"))


theme_niwot <- function(){
    theme_bw() +
        theme(text = element_text(family = "Times"),
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black"),
              panel.border = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
              plot.title = element_text(size = 18, vjust = 1, hjust = 0),
              legend.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.position = c(0.95, 0.15),
              legend.key = element_blank(),
              legend.background = element_rect(color = "black",
                                               fill = "transparent",
                                               size = 2, linetype = "blank"))
}




# Funksjon - Liste med vektor dato og periode ------------------------------

dato_vec <- function( ar_periode) {
    
    l <- list()
    
    start <- paste0(str_sub( ar_periode[1], start = 1, end = 4), "-01-01" ) 
    end   <- paste0(str_sub( ar_periode[length(ar_periode)], start = 1, end = 4), "-12-01" )
    
    l[["periode"]] <- map2_chr( rep(ar_periode, each = 12), rep(c(1:12), times = length(ar_periode)), function(y,x) ifelse( x < 10, paste0(y,"0", x), paste0(y,x) ))
    
    l[["dato"]]    <- seq( from = as.Date( start) , to = as.Date( end ), by = "month" )
    
    l
}
