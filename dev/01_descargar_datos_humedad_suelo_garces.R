# Descargar todos los datos de humedad de suelo

library(progress)

fun_get_SM <- function(station_id,time_span = c("2020-01-01","2022-08-08")){
  pb$tick()
  getdataAGV_sm(station_id,time_span) |> 
    dplyr::mutate(station_id =station_id)
}

pb <- progress_bar$new(total = dim(dplyr::filter(estaciones_garces,tipo == 'Humedad_Suelo'))[1],
                       format = "  downloading [:bar] :percent eta: :eta")

data_sm_todas <- estaciones_garces |> 
  dplyr::filter(tipo == 'Humedad_Suelo') |>
  dplyr::pull(serial) |>
  purrr::map(purrr::possibly(fun_get_SM,otherwise = 'Error'))

data_humedad_suelo <- data_sm_todas[!grepl('Error',data_sm_todas)]
data_humedad_suelo <- dplyr::bind_rows(data_humedad_suelo)

data_humedad_suelo |> dplyr::distinct(station_id)

library(ggplot2)
data_humedad_suelo |> 
  dplyr::filter(station_id == 'z6-10086') |> 
  dplyr::mutate(datetime = lubridate::ymd_hm(datetime)) |> 
  ggplot(aes(datetime,SM,colour= as.factor(depth))) +
  geom_line() + #geom_point() +
  scale_x_datetime(date_breaks = "1 months",date_labels = '%Y-%m') +
  theme_bw()
