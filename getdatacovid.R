library('httr')
library('jsonlite')
library('dplyr')
library('lubridate')

get_from_url <- function(date_range,
                         Save = T) {
  url <-
    c('https://brasil.io/api/dataset/covid19/caso/data?format=json&page=')
  
  df <- data.frame()
  
  n <- 1
  for (i in c(1:n)) {
    url2 <- paste0(url, i)
    
    tryCatch({
      urls <- GET(url2)
    },
    
    finally =
      
      {
        df <-
          rbind(df, (fromJSON(rawToChar(urls$content), flatten = TRUE)$results))
      })
    
    n <- n + 1
  }
  
  df <- df %>% distinct()
  
  # As bases vem com 1 linha a mais por estado.
  # Apenas informando que é um estado.......
  df <- df %>% filter(!is.na(city))
  
  # Pegando o range de data definido
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  date_range <- as.Date(date_range, format = "%Y-%m-%d")
  df <- df %>% filter(date >= min(date_range), date < max(date_range))
  
  if (Save) {
    write.csv(df,
              paste0("coronabrasil", as.character(today()), ".csv"),
              row.names = F)
  }
  
  return(df)
}

# Exemplo
# date_range é um vetor do intervalo de data dos dados (no formato %Y-%m-%d)
# Save indica se quer salvar ou não (.csv)
# arquivo "coronabrasil2020-03-26.csv"

df <- get_from_url(date_range = c("2018-01-01", "2021-01-01"),
                   Save = TRUE)
