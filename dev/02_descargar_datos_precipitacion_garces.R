# Descarga datos de precipitacion estaciones garces

library(dplyr)
library(purrr)
library(progress)
library(agvAPI)

fun_down_pre <- function(station_id){
  pb$tick()
  data <- getDataAGV_clima(station_id = station_id, 
                   var = 'Precipitation',
                   time_span = c('2022-01-01','2022-08-10'))
  data |>  
    mutate(
      station_id = station_id,
      pre = data |> (\(x) {pull(x,2)})()
  ) |> 
    select(c(datetime,station_id,pre))
}

pb <- progress_bar$new(total = dim(estaciones_garces)[1],
                       format = "  downloading [:bar] :percent eta: :eta")

data_pre_garces <- estaciones_garces |> 
  #slice(sample(1:199,10)) |> 
  pull(serial) |> 
  purrr::map_df(purrr::possibly(fun_down_pre,otherwise = NULL))

saveRDS(data_pre_garces,file = '~/Descargas/data_precipitacion_2022.rds')
