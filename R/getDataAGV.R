#' Get raw list-column data from agviewer.com/api/v2
#'
#' @param station_id serial number of the station
#' @param time_span time_span character vector of dates (%Y-%m-%d) having c(start_date,end_date) 
#'
#' @return
#' @import  httr2
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr pull
#' @export
#'
getdataAGV <- function(station_id,time_span){
 
  resp <- request(api_url) |> 
    httr2::req_auth_basic(usrpass[1], usrpass[2]) |> 
    httr2::req_headers("Accept" = "application/json") |> 
    httr2::req_url_path_append(glue::glue('?stations={station_id}&start={time_span[1]}&end={time_span[2]}')) |> 
    httr2::req_perform() |> 
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |> 
    dplyr::pull(sensors) |> 
    as.data.frame() |> 
    tibble::as_tibble()
  
}
