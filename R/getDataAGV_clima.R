#' Get climatic data from agviewer.com/api/v1
#'
#' @param station_id serial number of the station
#' @param time_span character vector of dates (%Y-%m-%d) having c(start_date,end_date) 
#' @param var pattern of the variable name to be search via regular expression 
#'
#' @return
#' @importFrom dplyr across select if_any
#' @export
#'
#' @examples
#' data1 <- getDataAGV_clima(station_id ='z6-12948',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28')) 
#' data2 <- getDataAGV_clima(station_id ='0020500D',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28'))
#' data3 <- getDataAGV_clima(station_id ='00203581',var = 'Temperature', time_span = c("2022-07-27",'2022-07-28')) 

getDataAGV_clima <- function(station_id = 'z6-14410', var = 'Temperature', time_span = c('2022-07-20','2022-07-25')){

  time_span <- lubridate::as_datetime(time_span)
  resp <- getdataAGV(station_id = station_id, time_span =time_span)
  
  type <- stringr::str_extract(station_id,'z6|[:alpha:]|[:digit:]+')
  
  if (is.numeric(type)) out <- .parseAGV(resp,station_id,var) 
  
  if (type == 'z6') {
    out <- .parseAGV(resp,station_id,var,type = 'z6')
  } else if (is.character(type)) out <- .parseAGV(resp,station_id,var)
  
  stopifnot('Does not correspond to any station type' = !is.na(type))
  
  return(out)
}

.parseAGV <- function(resp,station_id,var,type='generica'){
  
  if(type == 'z6'){
    data_tim_mes <- resp |> 
      dplyr::filter(grepl('Atmos|ATMOS',name)) |> 
      dplyr::pull(data) |> 
      as.data.frame() |> 
      tibble::as_tibble() 
    
    fun_sel <- function(d,v){
      out <- d |> 
        dplyr::filter(grepl(v,description))
      stopifnot("Variable name match more than one or none" = nrow(out) == 1)
      out
    }
    
    data_out <- data_tim_mes |> 
      dplyr::pull(measurements) |> 
      purrr::map_df(fun_sel, {{ var }} )
    
    new_name <- data_out |> 
      dplyr::distinct(description) |> 
      dplyr::pull(description)
      
    data_out <- data_out |> 
      dplyr::select(2) |> 
      purrr::set_names(new_name) |> 
      dplyr::mutate(datetime = lubridate::ymd_hm(data_tim_mes$timestamp)) |> 
      dplyr::select(2:1) |> 
      tibble::as_tibble()
    
  } else {
    data_out <- resp |> 
      dplyr::filter(grepl(glue::glue('{var}|{tolower(var)}'),name)) 
    
    stopifnot("Variable name match more than one or none" = nrow(data_out) == 1)
    
    data_out <- data_out |> 
      dplyr::pull(data) |>
      as.data.frame() |> 
      tibble::as_tibble() |>  
      dplyr::pull(measurements) |> 
      purrr::map_df(function(d){
        d |> 
          tidyr::pivot_wider(names_from=description,values_from = value)
      }) 
    
    if (var == 'Temperature' | var == 'VPD') 
      data_out <- data_out |> 
      dplyr::filter(if_any(1,\(x) x =='avg')) |> 
      dplyr::select(1:2)
    
    datetime <- resp |> 
      dplyr::filter(grepl(glue::glue('{var}|{tolower(var)}'),name)) |> 
      dplyr::pull(data) |>
      as.data.frame() |> 
      tibble::as_tibble() |> 
      dplyr::pull(timestamp) 
    
    data_out <- dplyr::mutate(data_out,datetime = datetime) |> 
      dplyr::mutate(datetime = lubridate::ymd_hm(datetime)) |> 
      dplyr::relocate(datetime)
  }
  return(data_out)
}
