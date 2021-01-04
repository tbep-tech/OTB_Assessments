
# setup -------------------------------------------------------------------

library(tidyverse)
library(tbeptools)
library(sf)
library(lubridate)
library(patchwork)
library(readxl)
library(gridExtra)

prj <- 4326

# otb wq data, stations, segments -----------------------------------------

# OTB pts
otbsta <- stations %>%
  filter(bay_segment %in% 'OTB') %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

epcdata <- read_importwq('epcdata.xlsx', download_latest = T)

# sf object of chl in otb, subset to date range for modis data
otbepc <- epcdata %>%
  filter(bay_segment %in% 'OTB') %>%
  dplyr::select(SampleTime, bay_segment, epchc_station, chla) %>%
  mutate(date = as.Date(SampleTime)) %>%
  dplyr::group_by(date, bay_segment, epchc_station) %>%
  dplyr::summarise(chla = mean(chla, na.rm = T), .groups = 'drop') %>%
  na.omit %>%
  mutate(
    mo = month(date),
    yr = year(date)
  ) %>%
  left_join(stations, by = c('epchc_station', 'bay_segment')) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) %>%
  filter(date >= min(otbpic$date) & date <= max(otbpic$date))

# otb segment polygon
otbseg <- tbseg %>%
  dplyr::filter(bay_segment %in% 'OTB')

# otb segment averages
otbavg <- epcdata %>%
  anlz_avedat %>%
  .$mos %>%
  dplyr::filter(bay_segment %in% 'OTB') %>%
  spread(var, val) %>%
  dplyr::select(-mean_la)

# pinellas co data --------------------------------------------------------

# data from query here https://www.tampabay.wateratlas.usf.edu/datadownload/Filters.aspx

picdata <- read_excel('pinellascowq.xlsx')

# raw data as sf, otb clip
otbpic <- picdata %>%
  .[, 1:21] %>%
  dplyr::filter(Parameter %in% 'Chla_ugl') %>%
  select(StationName, StationID = Actual_StationID, Longitude = Actual_Longitude, Latitude = Actual_Latitude, date = SampleDate, chla = Result_Value) %>%
  mutate(
    date = as.Date(date),
    mo = month(date),
    yr = year(date),
    Longitude = ifelse(Longitude > 0, -1 * Longitude, Longitude)
  ) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) %>%
  .[otbseg, ]

# extract pinellas county by proximity ------------------------------------

dst <- 0.01
bffotbsta <- st_buffer(otbsta, dist = dst)
mapview(otbpic[bffotbsta, ]) + mapview(bffotbsta, col.regions = 'red')

otbpicavg <- otbpic %>%
  .[bffotbsta, ] %>%
  st_set_geometry(NULL) %>%
  group_by(yr, mo) %>%
  summarise(mean_chla = mean(chla, na.rm = T), .groups = 'drop')

out1 <- otbpicavg %>%
  inner_join(otbavg, by = c("mo", "yr")) %>%
  rename(
    pic_chla = mean_chla.x,
    epc_chla = mean_chla.y
  )

# OTB segment averages ----------------------------------------------------

otbpicavg <- otbpic %>%
  st_set_geometry(NULL) %>%
  group_by(yr, mo) %>%
  summarise(mean_chla = mean(chla, na.rm = T), .groups = 'drop')

out2 <- otbpicavg %>%
  inner_join(otbavg, by = c("mo", "yr")) %>%
  rename(
    pic_chla = mean_chla.x,
    epc_chla = mean_chla.y
  )

# some plots --------------------------------------------------------------

toplo1 <- out1

toplo2 <-  out1 %>%
  filter(mo %in% c(4, 5))

toplo3 <- out2

toplo4 <- out2 %>%
  filter(mo %in% c(4, 5))

xlb <- "Pinellas co. chl-a"
ylb <- "EPC chl-a"
ttl1 <- 'OTB station, EPC chl matched to monthly ave of Pinellas stations with spatial buffer'
ttl2 <- 'OTB segment ave, EPC chl matched to monthly ave of all Pinellas stations'

p1 <- ggplot(toplo1, aes(x = pic_chla, y = epc_chla)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(
    x = xlb,
    y = ylb,
    subtitle = 'All months'
  )

p2 <- ggplot(toplo2, aes(x = pic_chla, y = epc_chla)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(
    x = xlb,
    y = ylb,
    subtitle = 'April, May only'
  )

p3 <- ggplot(toplo3, aes(x = pic_chla, y = epc_chla)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(
    x = xlb,
    y = ylb,
    subtitle = 'All months'
  )

p4 <- ggplot(toplo4, aes(x = pic_chla, y = epc_chla)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  # facet_wrap(~epchc_station) +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  labs(
    x = xlb,
    y = ylb,
    subtitle = 'April, May only'
  )

p <- grid.arrange(
  arrangeGrob(p1, p2, top = grid::textGrob(ttl1), ncol = 2),
  arrangeGrob(p3, p4, top = grid::textGrob(ttl2), ncol = 2),
  ncol = 1
)

png('figures/epcvpinchl.png', height = 8, width = 8, units = 'in', res = 300)
plot(p)
dev.off()
