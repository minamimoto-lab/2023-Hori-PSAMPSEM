roundrobin <- function(data, key, unclass = FALSE){
  data_nest <-
    data %>%
    dplyr::rename(key = key) %>%
    dplyr::group_nest(key)
  
  grid <-
    base::expand.grid(Var1 = data_nest$key, Var2 = data_nest$key)
  
  if(unclass == TRUE){
    grid <-
      grid %>%
      base::subset(base::unclass(Var1) < base::unclass(Var2))
  }
  
  grid %>%
    tibble::as_tibble() %>%
    dplyr::left_join(data_nest %>% dplyr::rename(Var1 = key),
                     by = "Var1")%>%
    dplyr::left_join(data_nest %>% dplyr::rename(Var2 = key),
                     by = "Var2")
}