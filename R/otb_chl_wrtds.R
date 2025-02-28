library(WRTDStidal)
library(tbeptools)
library(tidyverse)
library(here)

# create model, skip to import object below ---------------------------------------------------

# water quality
wqdat <- epcdata %>%
  filter(yr > 1974 & yr < 2025) %>%
  filter(bay_segment == 'OTB') %>%
  rowwise() %>%
  mutate(
    date = as.Date(SampleTime),
    date = floor_date(date, unit = 'month')
  ) %>%
  ungroup() %>%
  select(-matches('\\_q$|^sd|^Temp|\\_Depth\\_m$|^Lat|^Lon|^yr$|^mo$|^SampleTime$')) %>%
  mutate(sal = (Sal_Top_ppth + Sal_Mid_ppth + Sal_Bottom_ppth) / 3) %>%
  group_by(bay_segment, date) %>%
  summarise(
    tn = median(tn, na.rm = T),
    chla = median(chla, na.rm = T),
    sal = median (sal, na.rm = T),
    .groups = 'drop'
  )

# models
otbmods <- wqdat %>%
  mutate(
    res = log(chla),
    res = ifelse(is.infinite(res), NA, res),
    lim = 0,
  ) %>%
  select(bay_segment, date, res, flo = sal, lim) %>%
  na.omit() %>%
  group_by(bay_segment) %>%
  nest() %>%
  mutate(
    mod = purrr::map(data, function(x){

      out <- as.data.frame(x) %>% tidal %>%
        modfit(flo_div = 40, fill_empty = T, tau = 0.5)

      return(out)

    })
  )

save(otbmods, file = here('data/otbmods.RData'), compress = 'xz')

# obs v predict plot --------------------------------------------------------------------------

load(file = here('data/otbmods.RData'))

toplo <- otbmods$mod[[1]]

p <- ggplot(toplo, aes(x = date)) +
  geom_point(aes(y = res, color = 'Observed'), size = 2, alpha = 0.5) +
  geom_line(aes(y = fit0.5, color = 'Predicted'), linewidth  = 1) +
  scale_color_manual(values = c('black', 'tomato1')) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top'
  ) +
  labs(
    y = 'ln-Chl',
    x = NULL,
    color = NULL
  )

png(here('figures/otb_obs_v_pred.png'), width = 6, height = 3, units = 'in', res = 300)
print(p)
dev.off()

# quarterly predicted v normalized ------------------------------------------------------------

load(file = here('data/otbmods.RData'))

toplo <- otbmods %>%
  filter(bay_segment %in% 'OTB') %>%
  select(-data) %>%
  mutate(
    mod = purrr::map(mod, function(x) {

      out <- prdnrmplot(x, annuals = F, plot = F, logspace = F)
      out <- full_join(out[[1]], out[[2]], by = 'date') %>%
        select(date, norm0.5 = nrms_value, fit0.5 = fits_value) %>%
        mutate(resid = fit0.5 - norm0.5)

      return(out)

    })
  ) %>%
  pull(mod) %>%
  .[[1]] %>%
  filter(date >= as.numeric(as.Date('2000-01-01'))) %>%
  mutate(
    yr = year(date),
    qrt = quarter(date),
    qrt = factor(qrt, levels = c('1', '2', '3', '4'), labels = c('JFM', 'AMJ', 'JAS', 'OND'))
  ) %>%
  group_by(yr, qrt) %>%
  summarise(
    resid = mean(resid, na.rm = T),
    norm0.5 = mean(norm0.5, na.rm = T),
    fit0.5 = mean(fit0.5, na.rm = T),
    .groups = 'drop'
  )

#y axis label for plots
ylabs1 <- expression(paste('Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')'))

p <- ggplot(toplo, aes(x = yr)) +
  geom_point(aes(y = fit0.5, alpha=0.7), colour = 'tomato1', size = 3) +
  geom_line(aes(y = norm0.5), colour = '#00806E', alpha = 0.9, size = 1) +
  scale_y_continuous(ylabs1) +
  facet_wrap(~qrt, ncol = 4, scales = 'free_x') +
  theme_bw() +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

png(here('figures/prd_v_nrm_qrt.png'), width = 8, height = 3, units = 'in', res = 300)
print(p)
dev.off()

# contour plots -------------------------------------------------------------------------------

load(file = here('data/otbmods.RData'))

toplo <- otbmods %>%
  filter(bay_segment == 'OTB') %>%
  pull(mod) %>%
  .[[1]]

p <- gridplot(toplo, floscl = F, logspace = F, years = c(2000, 2024), month = 'all', allflo = T)

png(here('figures/otb_contour.png'), width = 6, height = 3, units = 'in', res = 300)
print(p)
dev.off()

p <- gridplot(toplo, floscl = F, logspace = F, years = c(2000, 2024), month = c(5, 6, 7, 8, 9, 10), allflo = T, ncol = 6) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

png(here('figures/otb_contour_wet.png'), width = 6, height = 3, units = 'in', res = 300)
print(p)
dev.off()
