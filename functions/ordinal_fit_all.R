ordinal_fit_all <- function(xVar, yVar, df){
        
        formula <- 
                formula(paste0(yVar, '~', 'scale(', xVar, ')'))
        
        pValue <-
                clm(
                        formula,
                        data = df
                ) %>% 
                summary() %>% 
                extract("coefficients") %>%
                extract2('coefficients') %>% 
                as_tibble() %>% 
                select("Pr(>|z|)") %>%
                dplyr::slice(n())
        
        return(pValue)
        
}