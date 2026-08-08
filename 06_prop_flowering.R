# load libraries--------------
library(raster)
library(sp)
library(lubridate)
library(plyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# load data-------------------
msg <- read.csv("data/raw_data/ObsLevel_Sept22.csv")
msg_coords <- msg
coordinates(msg_coords) <- ~ long_new2 + lat_new2

dem_stack <- stack("C://Users/krish/Documents/SeasonWatch/MSG/other_data/env_vars/dem_stack")
lights_KL <- raster("C://Users/krish/Documents/SeasonWatch/MSG/other_data/env_vars/lights_KL")
dem_vals <- raster::extract(dem_stack, msg_coords)
light_vals <- raster::extract(lights_KL, msg_coords)

msg <- cbind(msg, dem_vals, light_vals)

sw_2014 <- read.csv("data/modified_data/sw_msg_from2014.csv")
sw_spat <- msg

sw_spat$Date <- sw_2014$Date2[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$day <- sw_2014$date[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$month <- sw_2014$month[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$year <- sw_2014$year[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$User_Tree_id <- sw_2014$User_Tree_id[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$User_id <- sw_2014$User_id[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat$is.KL2 <- sw_2014$is.KL2[match(sw_spat$Observation_ID, sw_2014$Observation_ID)]
sw_spat <- sw_spat[sw_spat$is.KL2 == 1, ]


# sw_spat_forcor <- ddply(sw_spat, .(Species_name, User_Tree_id), summarise,
#   lat = mean(lat_new2), long = mean(long_new2),
#   elev = mean(elev), slope = mean(slope),
#   aspect = mean(aspect), light = mean(light_vals),
#   pcp = mean(FNSum_PCP), maxtemp = mean(FNMean_MaxTemp),
#   SM = mean(FNMean_MeanSM), DryDays = mean(FNSum_ConsDryDays),
#   mn_pcp = mean(MNSum_PCP), mn_maxtemp = mean(MNMean_MaxTemp),
#   mn_SM = mean(MNMean_MeanSM), mn_DryDays = mean(MNSum_ConsDryDays),
#   qr_pcp = mean(QrSum_PCP), qr_maxtemp = mean(QrMean_MaxTemp),
#   qr_SM = mean(QrMean_MeanSM), qr_DryDays = mean(QrSum_ConsDryDays)
# )

# spatcor <- cor(sw_spat_forcor[, 3:8], use = "complete.obs")

# sw_fn_cor <- ddply(sw_spat, .(Grid_ID, month), summarise,
#   fn_pcp = mean(FNSum_PCP), fn_maxtemp = mean(FNMean_MaxTemp),
#   fn_SM = mean(FNMean_MeanSM), fn_DryDays = mean(FNSum_ConsDryDays),
#   mn_pcp = mean(MNSum_PCP), mn_maxtemp = mean(MNMean_MaxTemp),
#   mn_SM = mean(MNMean_MeanSM), mn_DryDays = mean(MNSum_ConsDryDays),
#   qr_pcp = mean(QrSum_PCP), qr_maxtemp = mean(QrMean_MaxTemp),
#   qr_SM = mean(QrMean_MeanSM), qr_DryDays = mean(QrSum_ConsDryDays)
# )

# fn_cor <- cor(sw_fn_cor[, 3:14])


# scale everything
# https://stackoverflow.com/questions/41766181/correct-way-to-scale-for-multilevel-regression-using-lmer-r

# sw_spat_rescale <- transform(sw_spat,
#   FNSum_PCP_cs = scale(FNSum_PCP),
#   FNMean_MaxTemp_cs = scale(FNMean_MaxTemp),
#   FNMean_MeanSM_cs = scale(FNMean_MeanSM),
#   FNSum_DryDays_cs = scale(FNSum_ConsDryDays),
#   MNSum_PCP_cs = scale(MNSum_PCP),
#   FNMean_MinTemp_cs = scale(FNMean_MinTemp),
#   FNMean_MeanTemp_cs = scale(FNMean_MeanTemp),
#   FNSum_SolarRad_cs = scale(FNSum_SolarRad),
#   MNMean_MaxTemp_cs = scale(MNMean_MaxTemp),
#   MNMean_MeanSM_cs = scale(MNMean_MeanSM),
#   MNSum_DryDays_cs = scale(MNSum_ConsDryDays),
#   QrSum_PCP_cs = scale(QrSum_PCP),
#   QrMean_MaxTemp_cs = scale(QrMean_MaxTemp),
#   QrMean_MeanSM_cs = scale(QrMean_MeanSM),
#   QrSum_DryDays_cs = scale(QrSum_ConsDryDays),
#   elev_cs = scale(elev), slope_cs = scale(slope),
#   light_cs = scale(light_vals), aspect_cs = scale(aspect),
#   lat_sc = scale(lat_new2), long_sc = scale(long_new2)
# )


# spat_cs_cor <- cor(sw_spat_rescale[, 54:59], use = "complete.obs")
# ggcorrplot::ggcorrplot(spat_cs_cor, lab = T, type = "lower")

# # make the response variable a factor
# sw_spat_rescale$fl_intensity_fac <- factor(sw_spat_rescale$fl_intensity, levels = c("0", "1", "2"))
# sw_spat_rescale$fr_intensity_fac <- factor(sw_spat_rescale$fr_intensity, levels = c("0", "1", "2"))


sw_spat <- sw_spat %>%
  filter(is.KL2 == 1) %>%
  group_by(Species_name, User_Tree_id) %>%
  arrange(Date) %>%
  dplyr::mutate(
    fl_binary_prev = dplyr::lag(fl_binary, n = 1, default = NA, order_by = User_Tree_id),
    fl_intensity_prev = dplyr::lag(fl_intensity, n = 1, default = NA, order_by = User_Tree_id),
    fr_binary_prev = dplyr::lag(fr_binary, n = 1, default = NA, order_by = User_Tree_id),
    fr_intensity_prev = dplyr::lag(fr_intensity, n = 1, default = NA, order_by = User_Tree_id),
    diffdays = -as.numeric(difftime(dplyr::lag(Date, n = 1, default = NA, order_by = User_Tree_id), Date, units = "days"))
  ) %>%
  group_by(Species_name) %>%
  dplyr::mutate(
    mean_temp = as.vector(scale(FNMean_MeanTemp)),
    min_temp = as.vector(scale(FNMean_MinTemp)),
    max_temp = as.vector(scale(FNMean_MaxTemp)),
    solar_rad = as.vector(scale(FNSum_SolarRad)),
    precip = as.vector(scale(FNSum_PCP)),
    dry_days = as.vector(scale(FNSum_ConsDryDays)),
    soil_moisture = as.vector(scale(FNMean_MeanSM)),
    elev_cs = as.vector(scale(elev)),
    lights_cs = as.vector(scale(light_vals)),
    slope_cs = as.vector(scale(slope)),
    aspect_cs = as.vector((aspect))
  ) %>%
  ungroup()


sp_data_by_yr <- sw_spat %>%
  dplyr::mutate(year = as.integer(year)) %>%
  group_by(year, Species_name) %>%
  dplyr::summarise(
    n.obs = length(Species_name),
    n.tree.tot = length(unique(User_Tree_id)),
    n.tree.single = sum(table(User_Tree_id) == 1),
    n.obs.prev0 = sum(diffdays == 7 & fl_binary_prev == 0, na.rm = T),
    n.tree.prev0 = length(unique(User_Tree_id[which(diffdays == 7 & fl_binary_prev == 0)]))
  )

head(sp_data_by_yr)

# make data weekly-------------------

sw_spat$week <- week(sw_spat$Date)

sw_weeks <- sw_spat %>%
  dplyr::mutate(week = ifelse(week == 53, 52, week)) %>%
  # recode fl_binary to 0 for none and few and 1 for many
  dplyr::mutate(fl_binary2 = ifelse(fl_intensity == 2, 1, 0)) %>%
  group_by(Species_name, week, year) %>%
  # dplyr::summarise(prop.fl = sum(fl_binary == 1, na.rm = T) / length(fl_binary)) %>%
  dplyr::summarise(prop.fl = sum(fl_binary == 1, na.rm = T) / length(fl_binary)) %>%
  group_by(Species_name, week) %>%
  dplyr::summarise(
    mean.prop.fl = mean(prop.fl),
    sd.prop.fl = sd(prop.fl),
    low = mean.prop.fl - sd.prop.fl,
    high = mean.prop.fl + sd.prop.fl
  )

head(sw_weeks)

# summaries of climate variables for each week
sw_weeks_clim <- sw_spat %>%
  dplyr::mutate(week = ifelse(week == 53, 52, week)) %>%
  group_by(Species_name, week, year) %>%
  dplyr::summarise(
    prop.fl = sum(fl_binary == 1, na.rm = T) / length(fl_binary),
    mean_temp = mean(FNMean_MeanTemp),
    min_temp = mean(FNMean_MinTemp),
    max_temp = mean(FNMean_MaxTemp),
    solar_rad = mean(FNSum_SolarRad),
    precip = mean(FNSum_PCP),
    dry_days = mean(FNSum_ConsDryDays),
    soil_moisture = mean(FNMean_MeanSM),
    elev = mean(elev),
    lights = mean(light_vals),
    slope = mean(slope),
    aspect = mean(aspect)
  )

# pairs plot of climate variables and proportion flowering
png("doc/display/pairs_plot.png", width = 8, height = 8, units = "in", res = 300)
pairs(sw_weeks_clim[, 3:13])
dev.off()
colnames(sw_weeks_clim)

# weekly precip and temp across the years
weekly_precip <- ggplot(sw_weeks_clim %>% filter(Species_name == "Mangifera indica"), aes(x = week, y = precip, group = year, color = factor(year))) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Week", y = "Precipitation", title = "Weekly precipitation across the years") +
  theme_minimal()

weekly_temp <- ggplot(sw_weeks_clim %>% filter(Species_name == "Mangifera indica"), aes(x = week, y = mean_temp, group = year, color = factor(year))) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Week", y = "Mean temperature", title = "Weekly mean temperature across the years") +
  theme_minimal()

weekly_solar <- ggplot(
  sw_weeks_clim %>% filter(Species_name == "Mangifera indica"),
  aes(x = week, y = solar_rad, group = year, color = factor(year))
) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Week", y = "Solar radiation", title = "Weekly solar radiation across the years") +
  theme_minimal()

weekly_soilm <- ggplot(
  sw_weeks_clim %>% filter(Species_name == "Mangifera indica"),
  aes(x = week, y = soil_moisture, group = year, color = factor(year))
) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "Week", y = "Soil moisture", title = "Weekly soil moisture across the years") +
  theme_minimal()

library(patchwork)

layout <- "
AB
CD
"

png("doc/display/weekly_clim.png", width = 16, height = 16, units = "in", res = 300)
weekly_precip + weekly_temp + weekly_soilm + weekly_solar
dev.off()

# weekly proportion flowering across the years
weekly_fl_jack <- ggplot(sw_weeks_clim %>% filter(Species_name == "Artocarpus heterophyllus"), aes(x = week, y = prop.fl, group = year, color = year)) +
  geom_line() +
  # geom_point() +
  labs(x = "Week", y = "Proportion flowering", title = "jackfruit") +
  theme_minimal()

weekly_fl_mango <- ggplot(sw_weeks_clim %>% filter(Species_name == "Mangifera indica"), aes(x = week, y = prop.fl, group = year, color = year)) +
  geom_line() +
  # geom_point() +
  labs(x = "Week", y = "Proportion flowering", title = "mango") +
  theme_minimal()

weekly_fl_tam <- ggplot(sw_weeks_clim %>% filter(Species_name == "Tamarindus indica"), aes(x = week, y = prop.fl, group = year, color = year)) +
  geom_line() +
  # geom_point() +
  labs(x = "Week", y = "Proportion flowering", title = "tamarind") +
  theme_minimal()

png("doc/display/weekly_fl.png", width = 12, height = 4, units = "in", res = 300)
weekly_fl_jack + weekly_fl_mango + weekly_fl_tam
dev.off()

# for each year, calculate when flowering starts and ends
sw_weeks_fl <- sw_spat %>%
  dplyr::mutate(week = ifelse(week == 53, 52, week)) %>%
  # recode fl_binary to 0 for none and few and 1 for many
  dplyr::mutate(fl_binary2 = ifelse(fl_intensity == 2, 1, 0)) %>%
  group_by(Species_name, week, year) %>%
  dplyr::summarise(prop.fl = sum(fl_binary2 == 1, na.rm = T) / length(fl_binary2)) %>%
  # first change week to circular
  dplyr::mutate(
    week_deg = 360 * week / 52,
    week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
    year2 = ifelse(week_deg < 180, year - 1, year)
  )
# plot weekly proportion of flowering on a circle
ggplot(sw_weeks_fl, aes(x = week_deg, y = prop.fl, group = year, col = year)) +
  geom_line() +
  coord_polar() +
  facet_wrap(~Species_name) +
  theme_minimal() +
  labs(x = "Week", y = "Proportion flowering", title = "Weekly proportion flowering") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sw_year_fl <- sw_weeks_fl %>%
  # rotate degrees to start from September
  dplyr::mutate(
    week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
    # change year to match week_deg2
    year2 = ifelse(week_deg < 180, year - 1, year)
  ) %>%
  group_by(Species_name, year2) %>%
  dplyr::summarise(
    start_week = min(week_deg2[which(prop.fl > 0.2)], na.rm = T),
    end_week = max(week_deg2[which(prop.fl > 0.2)], na.rm = T),
    peak_week = week_deg2[which.max(prop.fl)],
    peak_prop_fl = max(prop.fl, na.rm = T),
  )

# plot weekly proportion of flowering on a circle
ggplot(sw_weeks_fl, aes(x = week_deg2, y = prop.fl, group = year2, col = year2)) +
  geom_line() +
  coord_polar() +
  facet_wrap(~Species_name) +
  theme_minimal() +
  labs(x = "Week", y = "Proportion flowering", title = "Weekly proportion flowering (phase shifted)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


jack_2014 <- sw_weeks_fl %>%
  filter(Species_name == "Artocarpus heterophyllus") %>%
  filter(year == 2014)

# distribution of observations across trees-------------------------

# how many trees with more than 1 observation?
sw_notcas <- sw_spat %>%
  group_by(Species_name, User_Tree_id) %>%
  dplyr::summarise(n.obs = length(User_Tree_id)) %>%
  filter(n.obs > 1) %>%
  ungroup() %>%
  dplyr::mutate(spname = ifelse(Species_name == "Artocarpus heterophyllus", "jackfruit",
    ifelse(Species_name == "Mangifera indica", "mango", "tamarind")
  ))

# plot distribution of number of observations for each species
sw_notcas_plot <- ggplot(sw_notcas, aes(
  x = n.obs,
  col = Species_name, fill = Species_name
)) +
  # geom_density(linewidth = 1.5) +
  geom_histogram() +
  # add rug
  # geom_rug() +
  facet_wrap(~spname) +
  scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  guides(col = "none", fill = "none") +
  labs(x = "Number of observations", y = "Number of trees", title = "Distribution of number of observations") +
  theme_minimal()

png("doc/display/distribution_regular.png", width = 8, height = 6, units = "in", res = 300)
sw_notcas_plot
dev.off()

sw_notcas_sum <- sw_notcas %>%
  group_by(Species_name) %>%
  dplyr::summarise(
    n.trees = length(User_Tree_id),
    mean.n.obs = mean(n.obs),
    sd.n.obs = sd(n.obs),
    max.n.obs = max(n.obs),
    min.n.obs = min(n.obs)
  )

# what time of year did flowering onset occur?
sw_spat <- readRDS("data/modified_data/sw_spat.RDS")
start_nomiss <- readRDS("data/modified_data/start_nomiss.RDS")

start_nomiss <- start_nomiss %>%
  dplyr::mutate(
    week = week(Date),
    week = ifelse(week == 53, 52, week),
    year = year(Date)
  )

library(tidyverse)
jack_start <- start_nomiss %>%
  filter(Species_name == "Artocarpus heterophyllus")
mango_start <- start_nomiss %>%
  filter(Species_name == "Mangifera indica")
tam_start <- start_nomiss %>%
  filter(Species_name == "Tamarindus indica")

# plot flowering onset with week for each species
# create a ridge plot
library(ggridges)
onset_dist <- ggplot(start_nomiss %>% filter(fl_binary == 1), aes(x = week, y = factor(year, levels = unique(year)), fill = Species_name)) +
  geom_density_ridges(alpha = 0.5) +
  facet_wrap(~Species_name) +
  guides(fill = "none") +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Week", y = "Species",
    title = "Flowering onset (switch from non-flowering to flowering)"
  ) +
  theme_minimal()

png("doc/display/onset_dist.png", width = 8, height = 8, units = "in", res = 300)
onset_dist
dev.off()

# distribution of flowering onset across latitude
onset_lat <- ggplot(start_nomiss %>% filter(fl_binary == 1), aes(
  x = week, y = lat_new2, fill = stat_count(),
  col = Species_name
)) +
  # make a heatmap
  # geom_density_2d() +
  geom_tile() +
  geom_point() +
  facet_wrap(~Species_name) +
  scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Latitude", y = "Week",
    title = "Flowering onset (switch from non-flowering to flowering)"
  ) +
  theme_minimal()


# circular statistics-------------------
library(circular)

# convert to circular using as.circular
sw_weeks_fl$week_deg2 <- circular::circular(sw_weeks_fl$week_deg2, type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")


sw_weeks_fl_circsum <- sw_weeks_fl %>%
  #   group_by(Species_name) %>%
  #   dplyr::mutate(circ.mean.allyrs = circular::weighted.mean.circular(week_deg2, w = prop.fl, na.rm = T))
  # ungroup() %>%
  group_by(year2, Species_name) %>%
  dplyr::summarise(
    circ.mean = circular::weighted.mean.circular(week_deg2, w = prop.fl, na.rm = T),
    circ.sd = circular::sd.circular(week_deg2, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(Species_name) %>%
  dplyr::mutate(
    circ.mean.allyrs = circular::mean.circular(circ.mean, na.rm = T)
  ) %>%
  ungroup()

# plot circular mean and sd
sw_circ_plot <- ggplot(sw_weeks_fl_circsum, aes(x = circ.mean, fill = Species_name)) +
  geom_histogram() +
  coord_polar() +
  facet_wrap(~Species_name) +
  scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Circular mean",
    title = "Circular mean of flowering onset"
  ) +
  # add a line for the mean
  geom_vline(aes(xintercept = circ.mean.allyrs), linetype = "dashed", color = "black") +
  guides(fill = "none") +
  theme_minimal()

png("doc/display/circ_mean.png", width = 12, height = 8, units = "in", res = 300)
sw_circ_plot
dev.off()


# timeseries with trees that have max observations-----------
tree_100 <- sw_notcas %>%
  filter(n.obs >= 100) %>%
  dplyr::mutate(tree = paste0(Species_name, "_", User_Tree_id))

sw_spat_100 <- sw_spat %>%
  dplyr::mutate(tree = paste0(Species_name, "_", User_Tree_id)) %>%
  filter(tree %in% tree_100$tree)

# how many trees have onset observations every year?--------

onset_every <- start_nomiss %>%
  filter(fl_binary == 1) %>%
  group_by(Species_name, User_Tree_id) %>%
  dplyr::summarise(n.years = length(unique(year))) %>%
  ungroup() %>%
  dplyr::mutate(spname = ifelse(Species_name == "Artocarpus heterophyllus", "jackfruit",
    ifelse(Species_name == "Mangifera indica", "mango", "tamarind")
  ))

onset_year_plot <- ggplot(onset_every, aes(x = n.years, fill = Species_name)) +
  geom_histogram() +
  facet_wrap(~spname) +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Number of years where flowering onset is recorded for each tree",
    y = "Count",
    title = "Histogram of trees with onset record"
  ) +
  guides(fill = "none") +
  theme_minimal()

png("doc/display/onset_year_plot.png", width = 8, height = 6, units = "in", res = 300)
onset_year_plot
dev.off()


# how many trees have uninterrupted weekly observations?-----------
uninterrupted <- sw_spat %>%
  group_by(Species_name, User_Tree_id) %>%
  dplyr::mutate(
    first.obs = min(Date),
    last.obs = max(Date)
  ) %>%
  group_by(Species_name, User_Tree_id, year) %>%
  dplyr::summarise(
    n.weeks = length(unique(week)),
    first.obs = first(first.obs),
    last.obs = last(last.obs)
  ) %>%
  # filter(n.weeks >= 52)
  filter(n.weeks >= 50)


uninterrupted_plot <- ggplot(uninterrupted, aes(x = year, fill = Species_name)) +
  geom_bar() +
  facet_wrap(~Species_name) +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Number of trees with uninterrupted observations for each year",
    y = "Count",
    title = "Histogram of trees with uninterrupted observations"
  ) +
  guides(fill = "none") +
  theme_minimal()


# plot start and end dates
uninterrupted_dates <- ggplot(uninterrupted, aes(x = as.Date(first.obs), y = as.Date(last.obs), col = Species_name)) +
  stat_density_2d_filled(alpha = 0.66) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Species_name) +
  scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "First observation date",
    y = "Last observation date",
    title = "First and last observation dates for trees with uninterrupted observations"
  ) +
  # axes as dates
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +
  scale_y_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +
  guides(col = "none") +
  theme_minimal()

png("doc/display/uninterrupted_dates.png", width = 12, height = 6, units = "in", res = 300)
uninterrupted_dates
dev.off()

# how many trees have uninterrupted observations across all years?-----------
uninterrupted_all <- uninterrupted %>%
  group_by(Species_name, User_Tree_id) %>%
  dplyr::summarise(n.years = length(unique(year)))


uninterrupted_all_plot <- ggplot(uninterrupted_all, aes(x = n.years, fill = Species_name)) +
  geom_histogram() +
  facet_wrap(~Species_name) +
  scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
  labs(
    x = "Number of years with uninterrupted observations for each tree",
    y = "Count",
    title = "Histogram of trees with uninterrupted observations"
  ) +
  guides(fill = "none") +
  theme_minimal()

library(patchwork)
png("doc/display/uninterrupted_plot.png", width = 8, height = 8, units = "in", res = 300)
uninterrupted_plot / uninterrupted_all_plot
dev.off()
