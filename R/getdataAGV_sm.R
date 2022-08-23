#' Get soil moisture data from agviewer.com/api/v1
#'
#' @param station_id serial number of the station
#' @param time_span character vector of dates (%Y-%m-%d) having c(start_date,end_date) 
#'
#' @return
#' @export
#'
#' @examples
getdataAGV_sm <- function(station_id ='z6-08674', time_span= c(Sys.Date()-1,Sys.Date())){
  
  df <- getdataAGV(station_id,time_span = time_span)
  
  depths <- list(30,60,90)
  
  df <- df |> 
    dplyr::filter(grepl('TEROS 10|TEROS 12|GS1',name)) |> 
    dplyr::pull(data)
  
  data_out <-   df |> 
    purrr::map2_df(depths,function(d,depth){
      o <- d |> 
        dplyr::pull(measurements)
      
      o |> purrr::map_df(function(d3){
        d3 |> dplyr::mutate(depth=depth)
      })
    }) |> 
    dplyr::select(value,depth) |> 
    dplyr::relocate(depth) |> 
    dplyr::rename(SM = value)
  
  
  data_out |> 
    dplyr::mutate(datetime = df |> purrr::map(function(d3) d3 |> dplyr::pull(timestamp)) |> unlist()) |> 
    dplyr::relocate(datetime)
  
}
