t <-
        dfClean %>%
        select(grupos, row) %>% 
        separate(
                grupos,
                into = paste0('cols', seq(5))
        ) %>%
        mutate(
                across(
                        .cols = everything(), 
                        .fns = str_replace,
                        pattern = "(?<!u)s",
                        replacement = NA_character_
                ),
                across(
                        everything(),
                        str_replace,
                        'NetworK',
                        NA_character_
                )
        ) %>% 
        pivot_longer(cols = -row) %>% 
        drop_na() %>% 
        pivot_wider(
                id_cols = row,
                names_from = value,
                values_fn = \(x) as.numeric(length(list(x)) > 0),
                values_fill = 0
        )
      