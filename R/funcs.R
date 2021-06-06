load_est <- function(start = '2020-01-01', end = '2020-12-31', noaa_key){

  browser()
  # download NOAA UWS rainfall station data
  sp_rainfall <- rnoaa::ncdc(datasetid = "GHCND", stationid = "GHCND:USW00092806",
                             datatypeid = "PRCP", startdate = start, enddate = end,
                             limit = 500, add_units = TRUE, token = noaa_key)
  tia_rainfall <- rnoaa::ncdc(datasetid = "GHCND", stationid = "GHCND:USW00012842",
                       datatypeid = "PRCP", startdate = start, enddate = end,
                       limit = 500, add_units = TRUE, token = noaa_key)

  # convert rain data to inches
  sp_rain <- sp_rainfall$data %>%
    mutate(daily_in = (value/254),
           Date = as.Date(date))
  tia_rain <- tia_rainfall$data %>%
    mutate(daily_in = (value/254),
           Date = as.Date(date))

  # download USGS streamflow data
  hr <- dataRetrieval::readNWISdv("02303000", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(hr_flow = mean(Flow)*0.892998604)
  ar <- dataRetrieval::readNWISdv("02301500", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(ar_flow = mean(Flow)*0.892998604)
  lmr<- dataRetrieval::readNWISdv("02300500", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(lmr_flow = mean(Flow)*0.892998604)
  bkr<- dataRetrieval::readNWISdv("02307359", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(bkr_flow = mean(Flow)*0.892998604)
  wl <- dataRetrieval::readNWISdv("02300042", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(wl_flow = mean(Flow)*0.892998604)
  mr <- dataRetrieval::readNWISdv("02299950", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::summarise(mr_flow = mean(Flow)*0.892998604)

  # Bay Segment hydrologic annual estimates
  HB <- data.frame(bs="Hillsborough Bay", stringsAsFactors = FALSE)
  HB$est <- 197.08+(1.84*ar$ar_flow)+(1.91*hr$hr_flow)

  OTB <- data.frame(bs="Old Tampa Bay", stringsAsFactors = FALSE)
  OTB$est <- 154.22+(8.12*bkr$bkr_flow)+(6.73*tia_rain$sum)

  MTB <- data.frame(bs="Middle Tampa Bay", stringsAsFactors = FALSE)
  MTB$est <- -13.78+(1.64*lmr$lmr_flow)+(8.68*sp_rain$sum)

  LTB <- data.frame(bs="Lower Tampa Bay", stringsAsFactors = FALSE)
  LTB$est <- 87.08+(3.69*sp_rain$sum)+(0.79*wl$wl_flow)+(0.62*mr$mr_flow)

  out <- left_join(tia_rain, bkr, by=c("Date")) %>%
    select(Date, daily_in, bkr_flow) %>%
    mutate(hyd_est = (154.22+(8.12*bkr_flow)+(6.73*daily_in))/365) %>%
    drop_na(hyd_est)

}
