# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)

# READ FILE

df <- 
        read_delim(
                "dataSets/satepsifav202307031424.txt", 
                delim = "+", 
                escape_double = FALSE, 
                col_names = FALSE, 
                trim_ws = TRUE) %>% 
        select(X2) %>% 
        rename('text' = 'X2')
        

dfSplit <-
        df %>%
        rowwise() %>% 
        mutate(
                nome = str_split(text, pattern = 'Autores')[[1]][[1]],
                autor = str_split(text, pattern = 'Editora')[[1]][[1]],
                autor = str_split(autor, pattern = 'Autores')[[1]][2],
                editora = str_split(text, pattern = 'Construto')[[1]][[1]],
                editora = str_split(editora, pattern = 'Editora')[[1]][2],
                construto = str_split(text, pattern = 'Público Alvo')[[1]][[1]],
                construto = str_split(construto, pattern = 'Construto')[[1]][2],
                pub.alvo = str_split(text, pattern = 'Idade da amostra de normatização')[[1]][[1]],
                pub.alvo = str_split(pub.alvo, pattern = 'Público Alvo')[[1]][2],
                idade.amostra = str_split(text, pattern = 'Aplicação')[[1]][[1]],
                idade.amostra = str_split(idade.amostra, pattern = 'Idade da amostra de normatização')[[1]][2],
                aplicacao = str_split(text, pattern = 'Correção')[[1]][[1]],
                aplicacao = str_split(aplicacao, pattern = 'Aplicação')[[1]][2],
                correcao = str_split(text, pattern = 'Data aprovação')[[1]][[1]],
                correcao = str_split(correcao, pattern = 'Correção')[[1]][2],
                data.aprovacao = str_split(text, pattern = 'Prazo dos estudos de normatização')[[1]][[1]],
                data.aprovacao = str_split(data.aprovacao, pattern = 'Data aprovação')[[1]][2],
                prazo.norm = str_split(text, pattern = 'Prazo dos estudos de validade')[[1]][[1]],
                prazo.norm = str_split(prazo.norm, pattern = 'Prazo dos estudos de normatização')[[1]][2],
                prazo.val = str_split(text, pattern = 'Prazo dos estudos de validade')[[1]][2]
        ) %>% 
        select(-text) %>% 
        filter(
                str_detect(nome, 'Favorável')
        )


const <-
        dfSplit %>%
        mutate(
                cList = str_split(construto, ',')
        ) %>% 
        unnest(cList) %>% 
        pivot_wider(
                names_from = cList, 
                values_from = cList, 
                values_fn = length,
                values_fill = 0
        )

write_csv(dfSplit, 'satepsiFavoravel.csv')

write_csv(const, 'cons.csv')
        


     
