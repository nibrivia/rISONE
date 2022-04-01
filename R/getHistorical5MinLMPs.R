#' get Five Minute System Load For Day
#' 
#' May need to add \code{config = config(ssl.verifypeer = FALSE)} if SSL cert cant be found.
#' 
#' @param day date in \code{'\%Y-\%m-\%d'}, or object coercible to class \code{Date}
#' @param user user account
#' @param location_id pnode ID for location
#' @param password password
#' @param out.tz output timezone.
#' @param ... passed to httr functions.
#' @importFrom lubridate mdy_hms
#' @importFrom httr GET authenticate accept_json config
#' @importFrom RJSONIO fromJSON
#' @export
getFiveMinuteLMPsForDayAtLocation <- function(day = Sys.Date(), user = getOption(x = "ISO_NE_USER"), password = getOption(x = "ISO_NE_PASSWORD"), 
                                          location_id = 4000,
                                          out.tz = "America/New_York", ...){
  
  dd <- format(as.Date(day), "%Y%m%d")
  
  json <- get_path(path = paste0("/fiveminutelmp/day/", dd, "/location/", location_id), user = user, password = password, ...)
  
  dat <- do.call(what = "rbind", 
                 lapply(json$FiveMinLmps$FiveMinLmp, 
                        FUN = function(x) as.data.frame(x = x, stringsAsFactors = FALSE))) %>% 
    filter(Location == paste(location_id))
  
  dat$BeginDate <- lubridate::ymd_hms(dat$BeginDate, tz = out.tz)
  
  return(dat)
  
}


#' Get historical 5-minute LMPs for a date range
#' 
#' @inheritDotParams getFiveMinuteLMPsForDayAtLocation
#' @export
getFiveMinuteLMPsAtLocation <- function(start_date = Sys.Date(), end_date = Sys.Date(), ...) {
  print(start_date)
  print(end_date)
  seq(start_date, end_date, by = "1 day") %>% 
    lapply(function(d) { getFiveMinuteLMPsForDayAtLocation(day = d, ...)}) %>% 
    dplyr::bind_rows() %>% 
    dplyr::as_tibble()
}
