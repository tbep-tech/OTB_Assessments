library(dataRetrieval)
library(rnoaa)
library(purrr)
library(tidyverse)

start <- "2019-01-01"
end <- "2019-12-31"

tia_rainfall <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00012842",
                     datatypeid = "PRCP", startdate = start, enddate = end,
                     limit = 500, add_units = TRUE)

tia_rain <- tia_rainfall$data %>%
             mutate(daily_in = value/254,
                    Date = as.Date(date))

bkr<- readNWISdv("02307359", "00060", start, end) %>%
      renameNWISColumns() %>%
      mutate(bkr_flow = (Flow-(mean(Flow)))*0.892998604)

otb_hydr <- left_join(tia_rain, bkr, by=c("Date")) %>%
            select(Date, daily_in, bkr_flow) %>%
            mutate(hyd_est = 154.22+(8.12*bkr_flow)+(6.73*daily_in))

ggplot(otb_hydr, aes(x=Date, y=hyd_est)) +
  geom_point(alpha=0) +
  geom_hline(aes(yintercept = min(otb_hydr$hyd_est))) +
  geom_linerange(aes(x=Date, ymax=otb_hydr$hyd_est, ymin=min(otb_hydr$hyd_est)))
