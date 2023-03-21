mosaic_plot <- function(..., df){

        # Salva informação das variáveis
        
        varNames <- tibble(...)
        
        # Plota o gráfico
        
        df %>%
                ggplot() + 
                geom_mosaic(
                        aes_string(
                                x = paste0(
                                        'product(',
                                        varNames$yVar,
                                        ', ',
                                        varNames$xVar,
                                        ')'
                                ),
                                fill = paste0(varNames$yVar)
                        )
                        
                ) +
                scale_fill_viridis_d(name = "", option = 'A') + 
                theme_minimal() + 
                theme(legend.position = "right",
                      legend.justification = "right",
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.text = element_text(colour = "azure4"),
                      plot.title = element_text(size = 18,
                                                colour = "azure4",
                                                face = "bold"),
                      plot.title.position = "plot",
                      legend.text = element_text(colour = "azure4")
                )
    
        
}