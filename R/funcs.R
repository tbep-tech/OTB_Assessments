hydroload_est <- function(yrsel, noaa_key){

  start <- paste0(yrsel, "-01-01")
  end <- paste0(yrsel, "-12-31")

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
    dplyr::mutate(hr_flow = Flow * 3.05119225)
  ar <- dataRetrieval::readNWISdv("02301500", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::mutate(ar_flow = Flow * 3.05119225)
  lmr<- dataRetrieval::readNWISdv("02300500", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::mutate(lmr_flow = Flow * 3.05119225)
  bkr<- dataRetrieval::readNWISdv("02307359", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::mutate(bkr_flow = Flow * 3.05119225)
  wl <- dataRetrieval::readNWISdv("02300042", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::mutate(wl_flow = Flow * 3.05119225)
  mr <- dataRetrieval::readNWISdv("02299950", "00060", start, end) %>%
    dataRetrieval::renameNWISColumns() %>%
    dplyr::mutate(mr_flow = Flow * 3.05119225)

  # Bay Segment hydrologic annual estimates
  HB <- full_join(ar, hr, by = 'Date') %>%
    mutate(
      bs = 'HB',
      est = 197.08+(1.84*ar_flow)+(1.91*hr_flow)
      ) %>%
    select(bs, Date, est)

  OTB <- left_join(tia_rain, bkr, by = 'Date') %>%
    mutate(
      est = 154.22+(8.12*bkr_flow)+(6.73*daily_in),
      bs = 'OTB'
    ) %>%
    select(bs, Date, est)

  MTB <- left_join(sp_rain, lmr, by = 'Date') %>%
    mutate(
      bs = 'MTB',
      est = -13.78+(1.64*lmr_flow)+(8.68*daily_in),
    ) %>%
    select(bs, Date, est)

  LTB <- left_join(sp_rain, wl, by = 'Date') %>%
    left_join(mr, by = 'Date') %>%
    mutate(
      bs = 'LTB',
      est = 87.08+(3.69*daily_in)+(0.79*wl_flow)+(0.62*mr_flow),
    ) %>%
    select(bs, Date, est)

  out <- dplyr::bind_rows(HB, OTB, MTB, LTB)

  return(out)

}

# get chlorophyll boxplot and hydroload plot
chlhydplo_fun <- function(bay_segment, chldat, hydroests, yrsel){

  yrrng <- c(1975, 2020)
  ptsz <- 2

  # default targets from data file
  trgs <- targets

  # monthly averages
  aves <- anlz_avedat(chldat) %>%
    .$'mos' %>%
    dplyr::filter(var %in% 'mean_chla') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::mutate(
      var = 'yval',
      mo = month(mo, label = T)
    )

  # axis label
  axlab <- expression("Mean Monthly Chlorophyll-a ("~ mu * "g\u00B7L"^-1 *")")

  # get lines to plot
  thrnum <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(chla_thresh)

  # threshold label
  trglab <- paste(thrnum, "~ mu * g%.%L^{-1}")

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)

  # toplo1 is all but current year
  toplo1 <- aves %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(!yr %in% yrsel) %>%
    mutate(
      yrcat = case_when(
        yr < 2008 ~ '1975-2007',
        yr >= 2008 ~ '2008-2019'
      )
    )

  # toplo2 is current year
  toplo2 <- aves %>%
    dplyr::filter(yr %in% yrsel)

  p1 <- ggplot() +
    geom_boxplot(data = toplo1, aes(x = mo, y = val), outlier.colour = NA) +
    geom_beeswarm(data = toplo1, aes(x = mo, y = val, colour = yrcat), size = ptsz, alpha = 0.6) +
    geom_point(data = toplo2, aes(x = mo, y = val, fill = as.character(yrsel)), pch = 21, color = 'black', size = 4, alpha = 0.7) +
    geom_hline(aes(yintercept = thrnum, linetype = 'Annual Threshold'), colour = 'blue') +
    geom_text(aes(x = factor('Jan'), thrnum), parse = TRUE, label = trglab, hjust = 0.2, vjust = -0.2, colour = 'blue') +
    labs(y = axlab, title = ttl) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background = element_rect(fill = '#ECECEC'),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
    ) +
    scale_fill_manual(values = 'black') +
    scale_linetype_manual(values = 'dotted') +
    guides(linetype = guide_legend(override.aes = list(colour = 'blue')))

  toplo3 <- hydroests %>%
    filter(bs %in% !!bay_segment)

  p2 <- ggplot(toplo3, aes(x=Date, y=est)) +
    geom_point(alpha=0) +
    geom_hline(aes(yintercept = min(est, na.rm = TRUE))) +
    geom_linerange(aes(x=Date, ymax=est, ymin=min(est, na.rm = TRUE))) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b',
                 limits = lubridate::floor_date(range(toplo3$Date), 'month')) +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = '#ECECEC'),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      legend.position = 'top',
      legend.background = element_rect(fill=NA),
      legend.key = element_rect(fill = '#ECECEC'),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
    ) +
    labs(
      y = "Hydrologic Load\n(mill m3)"
    )

  p1 + p2 + plot_layout(ncol = 1, heights = c(1, 0.3))

}
