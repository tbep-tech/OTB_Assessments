
# setup -------------------------------------------------------------------

library(tidyverse)
library(raster)
library(tbeptools)
library(sf)
library(mapview)
library(lubridate)
library(foreach)
library(doParallel)
library(patchwork)

prj <- 4326
rstprj <- '+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R=6371007 +units=m +no_defs'

# modis chlorophyll file list ---------------------------------------------

fls <- data.frame(
  flsfl = list.files('~/Desktop/TBEP/RGCI_chlor/', full.names = T)
  ) %>%
  mutate(
    fls = basename(flsfl),
    yr = substr(fls, 2, 5),
    jul = substr(fls, 6, 8),
    tim = substr(fls, 9, 12),
    origin = paste0(yr, '-01-01'),
    dt = as.Date(as.numeric(jul) - 1, origin),
    dttm = ymd_hm(paste(dt, tim))
  ) %>%
  dplyr::select(flsfl, fls, dt, dttm)

# otb wq data, stations, segments -----------------------------------------

# OTB pts
otbsta <- stations %>%
  filter(bay_segment %in% 'OTB') %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) %>%
  st_transform(rstprj)

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
  st_transform(rstprj) %>%
  filter(date >= min(fls$dt) & date <= max(fls$dt))

# otb segment polygon
otbseg <- tbseg %>%
  dplyr::filter(bay_segment %in% 'OTB') %>%
  st_transform(rstprj)

# otb segment averages
otbavg <- epcdata %>%
  anlz_avedat %>%
  .$mos %>%
  dplyr::filter(bay_segment %in% 'OTB') %>%
  spread(var, val) %>%
  dplyr::select(-mean_la)

# extract chl from MODIS --------------------------------------------------

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 1)
registerDoParallel(cl)

# setup log file
strt <- Sys.time()

dts <- unique(otbepc$date)

# process, extract epc station points from monthly stack of all days in each month, average across stacks
res1 <- foreach(i = dts, .packages = c('raster', 'sf', 'dplyr', 'lubridate')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(i == dts), ' of ', length(dts), '\n')
  print(Sys.time() - strt)
  sink()

  # tmp obs
  tmpobs <- otbepc %>%
    dplyr::filter(date %in% i)

  # get modis
  rst <- fls %>%
    filter(year(dt) == year(i)) %>%
    filter(month(dt) == month(i)) %>%
    pull(flsfl)

  if(length(rst) == 0){
    tmpobs$chlamod <- NA
    return(tmpobs)
  }

  # get raster
  tmpmod <- raster::stack(rst)

  # extract chl, add to data
  tmpobs$chlamod <- raster::extract(tmpmod, tmpobs) %>% rowMeans(na.rm = T)

  return(tmpobs)

}

out1 <- do.call('rbind', res1) %>%
  filter(!is.na(chlamod))

# OTB segment modis average -----------------------------------------------

# setup parallel backend
ncores <- detectCores()
cl <- makeCluster(ncores - 1)
registerDoParallel(cl)

# setup log file
strt <- Sys.time()

dts <- unique(otbepc$date)

# process, extract epc station points from monthly stack of all days in each month, average across stacks
res2 <- foreach(i = dts, .packages = c('raster', 'sf', 'dplyr', 'lubridate', 'tbeptools')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(i == dts), ' of ', length(dts), '\n')
  print(Sys.time() - strt)
  sink()

  # get modis
  rst <- fls %>%
    filter(year(dt) == year(i)) %>%
    filter(month(dt) == month(i)) %>%
    pull(flsfl)

  if(length(rst) == 0)
    return(NA)

  # get raster value averages
  vals <- raster::stack(rst) %>%
    raster::crop(otbseg) %>%
    raster::calc(fun = mean, na.rm = T) %>%
    raster::getValues() %>%
    mean(na.rm = T)

  return(vals)

}

out2 <- data.frame(
  dts = dts,
  chlamod = unlist(res2)
  ) %>%
  mutate(
    mo = month(dts),
    yr = year(dts)
  ) %>%
  left_join(otbavg, by = c('mo', 'yr')) %>%
  rename(chla = mean_chla)

# some plots --------------------------------------------------------------

toplo1 <- out1

toplo2 <-  out1 %>%
  filter(mo %in% c(4, 5))

toplo3 <- out2 %>%
  filter(yr == 2019)

toplo4 <- out2 %>%
  filter(mo %in% c(4, 5))

xlb <- "Modis chl-a"
ylb <- "Obs chl-a"
ttl1 <- 'OTB station, obs chl matched to monthly ave of MODIS at station location'
ttl2 <- 'OTB segment ave, observed chl matched to monthly ave of MODIS for all pixels'

p1 <- ggplot(toplo1, aes(x = chlamod, y = chla)) +
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

p2 <- ggplot(toplo2, aes(x = chlamod, y = chla)) +
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

p3 <- ggplot(toplo3, aes(x = chlamod, y = chla)) +
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

p4 <- ggplot(toplo4, aes(x = chlamod, y = chla)) +
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

png('figures/obsvmodischl.png', height = 8, width = 8, units = 'in', res = 300)
plot(p)
dev.off()
