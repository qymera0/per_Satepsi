ind_plot_char <- function(df, y, title){
        
        df %>% 
                arrange(.data[[y]]) %>% 
                ggplot(aes_string(x = shQuote(''), fill = y)) +
                geom_bar(position = position_fill(reverse = TRUE), 
                         width = 0.6) +
                scale_y_continuous(labels = scales::percent) +
                coord_flip() +
                scale_fill_viridis_d(name = "") + 
                labs(X = "", y = "", title = paste(strwrap(title,
                                                           width = 50),
                                                   collapse = "\n")) + 
                theme_minimal() + 
                theme(legend.position = "bottom",
                      legend.justification = "right",
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.title = element_blank(),
                      axis.text = element_text(colour = "azure4"),
                      plot.title = element_text(size = 18,
                                                colour = "azure4",
                                                face = "bold"),
                      plot.title.position = "plot",
                      legend.text = element_text(colour = "azure4")
                )
}