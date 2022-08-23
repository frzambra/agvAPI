#' Get raw list-column data from agviewer.com/api/v2
#'
#' @param station_id serial number of the station
#' @param time_span time_span character vector of dates (%Y-%m-%d) having c(start_date,end_date) 
#'
#' @return
#' @export
#'
#' @examples
getdataAGV <- function(station_id,time_span){
  resp <- httr::GET(api_url,
                    query = list(
                      format = 'json',
                      stations = station_id,
                      start = time_span[1],
                      end =time_span[2]),
                    config = httr::config(ssl_verifypeer = FALSE),
                    httr::authenticate("agricolagarces", "user12345")) |> 
    httr::content("text",encoding = 'UTF-8') |>
    jsonlite::fromJSON() |> 
    dplyr::pull(sensors) |> 
    as.data.frame() |> 
    tibble::as_tibble()
  
}
