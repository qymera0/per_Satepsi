chi_Test_All <- function(xVar, yVar, df){
        
        contTable <-
                df %>% 
                select(all_of(xVar), all_of(yVar)) %>% 
                table()
        
        chi <- chisq.test(contTable, simulate.p.value = T)
        
        return(pvalue = chi$p.value)
        
}
