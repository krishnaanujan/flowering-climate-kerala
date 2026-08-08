# circular analysis of onset data

library(circular)
library(CircMLE) # For Hermans-Rasson test
library(tidyverse)

sw_spat <- readRDS("data/modified_data/sw_spat.RDS")
start_nomiss <- readRDS("data/modified_data/start_nomiss.RDS")

# for each year and week, calculate proportion of flowering individuals
sw_weeks_fl <- sw_spat %>%
    dplyr::mutate(
        week = lubridate::week(Date),
        week = ifelse(week == 53, 52, week)
    ) %>%
    # recode fl_binary to 0 for none and few and 1 for many
    dplyr::mutate(fl_binary2 = ifelse(fl_intensity == 2, 1, 0)) %>%
    group_by(Species_name, week, year) %>%
    dplyr::summarise(
        prop.fl = sum(fl_binary == 1, na.rm = T) / length(fl_binary),
        n.fl = sum(fl_binary == 1, na.rm = T),
        n.obs = length(fl_binary)
    ) %>%
    # first change week to circular
    dplyr::mutate(
        week_deg = 360 * week / 52,
        week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
        year2 = ifelse(week_deg < 180, year - 1, year)
    )

# n.obs statistics for species
obs.nums <- sw_weeks_fl %>%
    # remove 2022
    dplyr::filter(year2 != 2022) %>%
    dplyr::group_by(Species_name) %>%
    dplyr::summarise(
        n.obs.min = min(n.obs),
        n.obs.mean = mean(n.obs)
    )
obs.nums

# for each year and calculate proportion of individuals that showed flowering onset
sw_weeks_flon <- start_nomiss %>%
    dplyr::mutate(
        week = lubridate::week(Date),
        week = ifelse(week == 53, 52, week)
    ) %>%
    # recode fl_binary to 0 for none and few and 1 for many
    dplyr::mutate(fl_binary2 = ifelse(fl_intensity == 2, 1, 0)) %>%
    group_by(Species_name, week, year) %>%
    dplyr::summarise(
        prop.fl = sum(fl_binary == 1, na.rm = T) / length(fl_binary),
        n.fl = sum(fl_binary == 1, na.rm = T),
        n.obs = length(fl_binary)
    ) %>%
    # first change week to circular
    dplyr::mutate(
        week_deg = 360 * week / 52,
        week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
        year2 = ifelse(week_deg < 180, year - 1, year)
    )

head(sw_weeks_flon)

# n.obs statistics for species
obs.nums.on <- sw_weeks_flon %>%
    # remove 2022
    dplyr::filter(year2 != 2022) %>%
    dplyr::group_by(Species_name) %>%
    dplyr::summarise(
        n.obs.min = min(n.obs),
        n.obs.mean = mean(n.obs)
    )
obs.nums.on

# For both proportion of flowering and flowering onset
# calculate circular mean for each year and species using bootstrapping

# to calculate rayleigh test, we need a vector of angles
sw_weeks_fl_degs <- sw_weeks_fl <- sw_spat %>%
    dplyr::mutate(
        week = lubridate::week(Date),
        week = ifelse(week == 53, 52, week)
    ) %>%
    # first change week to circular
    dplyr::mutate(
        week_deg = 360 * week / 52,
        week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
        year2 = ifelse(week_deg < 180, year - 1, year),
        week_deg2 = as.circular(week_deg2, type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")
    ) %>%
    filter(fl_binary == 1) %>%
    dplyr::select(Species_name, week_deg2, year2)

# Rayleigh test
rayleigh_jack <- rayleigh.test(sw_weeks_fl_degs %>% filter(Species_name == "Artocarpus heterophyllus") %>% pull(week_deg2))
rayleigh_mango <- rayleigh.test(sw_weeks_fl_degs %>% filter(Species_name == "Mangifera indica") %>% pull(week_deg2))
rayleigh_tamarind <- rayleigh.test(sw_weeks_fl_degs %>% filter(Species_name == "Tamarindus indica") %>% pull(week_deg2))

# this takes too long
# # Hermans-Rasson test (using CircMLE)
# hr_jack <- HR_test(sw_weeks_fl_degs %>% filter(Species_name == "Artocarpus heterophyllus") %>% pull(week_deg2), iter = 1000)
# hr_mango <- HR_test(sw_weeks_fl_degs %>% filter(Species_name == "Mangifera indica") %>% pull(week_deg2), iter = 1000)
# hr_tamarind <- HR_test(sw_weeks_fl_degs %>% filter(Species_name == "Tamarindus indica") %>% pull(week_deg2), iter = 1000)

# cat("Empirical Hermans-Rasson test:\n")
# cat("T =", hr_result["Test statistic (T)"], "p =", hr_result["p-value"], "\n")

# bootstrapping-------------------------

# used code from Willig et al 2024 https://zenodo.org/records/10799004
# converted to R with the help of perplexity

fl.df <- sw_spat %>%
    dplyr::mutate(
        week = lubridate::week(Date),
        week = ifelse(week == 53, 52, week)
    ) %>%
    # first change week to circular
    dplyr::mutate(
        week_deg = 360 * week / 52,
        week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
        year2 = ifelse(week_deg < 180, year - 1, year)
    )


# Number of bootstrap populations
N_populations <- 1000
sps <- c("Artocarpus heterophyllus", "Mangifera indica", "Tamarindus indica")
yrs <- 2013:2021

# Choose option: 'minimum' or 'average'
# option <- "minimum" # or 'average'
option <- "average" # or 'average'

# sp <- "Artocarpus heterophyllus"
# yr <- 2014

# expand grid for sps and years
statdf <- expand.grid(Species_name = sps, year = yrs)
# make empty columns for the results
statdf$circmean <- NA
statdf$circmean_sd <- NA
statdf$z_rayleigh <- NA
statdf_z_rayleigh_sd <- NA
statdf$p_rayleigh <- NA
statdf$z2.5 <- NA
statdf$z50 <- NA
statdf$z97.5 <- NA
statdf$p2.5 <- NA
statdf$p50 <- NA
statdf$p97.5 <- NA
statdf$Q005 <- NA


for (sp in sps) {
    for (yr in yrs) {
        fl.df2 <- fl.df %>% filter(Species_name == sp & year2 == yr)

        week <- round((fl.df2$week_deg2), 2)
        fl <- fl.df2$fl_binary

        # Count individuals per week
        N_weeks <- as.numeric(table(week))
        week_unique <- round(as.numeric(names(table(week))), 2)
        N_min <- min(N_weeks)
        N_avg <- round(mean(N_weeks))

        N_count <- ifelse(option == "average", N_avg, N_min)

        set.seed(123)
        p_rayleigh <- numeric(N_populations)
        z_rayleigh <- numeric(N_populations)
        # p_HR <- numeric(N_populations)
        # T_HR <- numeric(N_populations)

        for (n in 1:N_populations) {
            week_thispop <- integer(0)
            fl_thispop <- integer(0)

            for (this_week in week_unique) {
                ind_week <- which(week == this_week)
                ind_week_random <- sample(ind_week, size = N_count, replace = TRUE)
                week_thispop <- c(week_thispop, week[ind_week_random])
                fl_thispop <- c(fl_thispop, fl[ind_week_random])
            }

            # Convert weeks to circular
            angle_thispop <- circular((week_thispop), type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")
            ind_fl <- which(fl_thispop == 1)

            # Rayleigh test
            if (length(ind_fl) > 0) {
                # circular mean
                mean_angle <- circular::mean.circular(angle_thispop[ind_fl], na.rm = TRUE)
                rayleigh_res <- rayleigh.test(angle_thispop[ind_fl])
                z_rayleigh[n] <- as.numeric(rayleigh_res$statistic)
                p_rayleigh[n] <- as.numeric(rayleigh_res$p.value)

                # # Hermans-Rasson test
                # hr_res <- HR_test(angle_thispop[ind_pregnant], iter = 1000)
                # T_HR[n] <- hr_res["Test statistic (T)"]
                # p_HR[n] <- hr_res["p-value"]
            } else {
                z_rayleigh[n] <- NA
                p_rayleigh[n] <- NA
                # T_HR[n] <- NA
                # p_HR[n] <- NA
            }
        }
        # # Rayleigh test summary
        # cat("Rayleigh test for", sp, "in", yr, "\n")
        # cat("z_rayleigh =", mean(z_rayleigh, na.rm = TRUE), "+/-", sd(z_rayleigh, na.rm = TRUE), "\n")
        # cat("z2.5% =", quantile(z_rayleigh, 0.025, na.rm = TRUE), "\n")
        # cat("z50% =", quantile(z_rayleigh, 0.5, na.rm = TRUE), "\n")
        # cat("z97.5% =", quantile(z_rayleigh, 0.975, na.rm = TRUE), "\n\n")

        # cat("p_rayleigh =", mean(p_rayleigh, na.rm = TRUE), "+/-", sd(p_rayleigh, na.rm = TRUE), "\n")
        # cat("2.5% =", quantile(p_rayleigh, 0.025, na.rm = TRUE), "\n")
        # cat("50% =", quantile(p_rayleigh, 0.5, na.rm = TRUE), "\n")
        # cat("97.5% =", quantile(p_rayleigh, 0.975, na.rm = TRUE), "\n")
        # cat("Q005 =", mean(p_rayleigh <= 0.05, na.rm = TRUE), "\n\n")

        # save these in a dataframe
        colnum <- which(statdf$Species_name == sp & statdf$year == yr)

        statdf$circmean[colnum] <- mean(mean_angle, na.rm = TRUE)
        statdf$circmean_sd[colnum] <- sd(mean_angle, na.rm = TRUE)
        statdf$z_rayleigh[colnum] <- mean(z_rayleigh, na.rm = TRUE)
        statdf$z_rayleigh_sd[colnum] <- sd(z_rayleigh, na.rm = TRUE)
        statdf$p_rayleigh[colnum] <- mean(p_rayleigh, na.rm = TRUE)
        statdf$z2.5[colnum] <- quantile(z_rayleigh, 0.025, na.rm = TRUE)
        statdf$z50[colnum] <- quantile(z_rayleigh, 0.5, na.rm = TRUE)
        statdf$z97.5[colnum] <- quantile(z_rayleigh, 0.975, na.rm = TRUE)
        statdf$p2.5[colnum] <- quantile(p_rayleigh, 0.025, na.rm = TRUE)
        statdf$p50[colnum] <- quantile(p_rayleigh, 0.5, na.rm = TRUE)
        statdf$p97.5[colnum] <- quantile(p_rayleigh, 0.975, na.rm = TRUE)
        statdf$Q005[colnum] <- mean(p_rayleigh <= 0.05, na.rm = TRUE)
    }
}

View(statdf)

# # save the results
write.csv(statdf, "models/circular/rayleigh_test_results.csv", row.names = FALSE)


# # Hermans-Rasson test summary
# cat("T_HR =", mean(T_HR, na.rm = TRUE), "+/-", sd(T_HR, na.rm = TRUE), "\n")
# cat("T2.5% =", quantile(T_HR, 0.025, na.rm = TRUE), "\n")
# cat("T50% =", quantile(T_HR, 0.5, na.rm = TRUE), "\n")
# cat("T97.5% =", quantile(T_HR, 0.975, na.rm = TRUE), "\n\n")

# cat("p_HR =", mean(p_HR, na.rm = TRUE), "+/-", sd(p_HR, na.rm = TRUE), "\n")
# cat("2.5% =", quantile(p_HR, 0.025, na.rm = TRUE), "\n")
# cat("50% =", quantile(p_HR, 0.5, na.rm = TRUE), "\n")
# cat("97.5% =", quantile(p_HR, 0.975, na.rm = TRUE), "\n")
# cat("Q005_HR =", mean(p_HR <= 0.05, na.rm = TRUE), "\n")

# plot stuff------------------------

statdf <- read.csv("models/circular/rayleigh_test_results.csv")
statdf$spname <- ifelse(statdf$Species_name == "Artocarpus heterophyllus", "jackfruit",
    ifelse(statdf$Species_name == "Mangifera indica", "mango", "tamarind")
)

# install.packages("devtools")
# devtools::install_github("eliocamp/tagger")

library(tagger)

# plot circular mean and sd
# spnames <- c("jackfruit", "mango", "tamarind")
sw_circ_plot <- ggplot(statdf %>% filter(year != 2013), aes(x = circmean, , y = z_rayleigh, fill = spname)) +
    geom_bar(stat = "identity", position = "dodge", width = 3) +
    coord_polar() +
    geom_segment(aes(y = 0, xend = circmean, yend = z_rayleigh), arrow = arrow(length = unit(0.3, "cm"))) +
    facet_wrap(~spname) +
    scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    tag_facets() +
    labs(
        x = "",
        y = "Rayleigh z statistic",
        title = "Circular mean of flowering status"
    ) +
    scale_x_continuous(
        breaks = seq(0, 330, by = 30),
        # labels = c("June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
        labels = c("July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June")
    ) +
    # add a line for the mean
    # geom_vline(aes(xintercept = circ.mean.allyrs), linetype = "dashed", color = "black") +
    guides(fill = "none") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12))

png("doc/display/circ_mean_boot.png", width = 12, height = 6, units = "in", res = 300)
sw_circ_plot
dev.off()

# repeat for flowering onset----------------------------------

flon.df <- start_nomiss %>%
    dplyr::mutate(
        week = lubridate::week(Date),
        week = ifelse(week == 53, 52, week)
    ) %>%
    # first change week to circular
    dplyr::mutate(
        week_deg = 360 * week / 52,
        week_deg2 = ifelse(week_deg < 180, week_deg + 180, week_deg - 180),
        year2 = ifelse(week_deg < 180, year - 1, year)
    )

# expand grid for sps and years
statdf <- expand.grid(Species_name = sps, year = yrs)
# make empty columns for the results
statdf$circmean <- NA
statdf$circmean_sd <- NA
statdf$z_rayleigh <- NA
statdf_z_rayleigh_sd <- NA
statdf$p_rayleigh <- NA
statdf$z2.5 <- NA
statdf$z50 <- NA
statdf$z97.5 <- NA
statdf$p2.5 <- NA
statdf$p50 <- NA
statdf$p97.5 <- NA
statdf$Q005 <- NA


for (sp in sps) {
    for (yr in yrs) {
        fl.df2 <- flon.df %>% filter(Species_name == sp & year2 == yr)

        week <- round((fl.df2$week_deg2), 2)
        fl <- fl.df2$fl_binary

        # Count individuals per week
        N_weeks <- as.numeric(table(week))
        week_unique <- round(as.numeric(names(table(week))), 2)
        N_min <- min(N_weeks)
        N_avg <- round(mean(N_weeks))

        N_count <- ifelse(option == "average", N_avg, N_min)

        set.seed(123)
        p_rayleigh <- numeric(N_populations)
        z_rayleigh <- numeric(N_populations)
        # p_HR <- numeric(N_populations)
        # T_HR <- numeric(N_populations)

        for (n in 1:N_populations) {
            week_thispop <- integer(0)
            fl_thispop <- integer(0)

            for (this_week in week_unique) {
                ind_week <- which(week == this_week)
                ind_week_random <- sample(ind_week, size = N_count, replace = TRUE)
                week_thispop <- c(week_thispop, week[ind_week_random])
                fl_thispop <- c(fl_thispop, fl[ind_week_random])
            }

            # Convert weeks to circular
            angle_thispop <- circular((week_thispop), type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")
            ind_fl <- which(fl_thispop == 1)

            # Rayleigh test
            if (length(ind_fl) > 0) {
                # circular mean
                mean_angle <- circular::mean.circular(angle_thispop[ind_fl], na.rm = TRUE)
                rayleigh_res <- rayleigh.test(angle_thispop[ind_fl])
                z_rayleigh[n] <- as.numeric(rayleigh_res$statistic)
                p_rayleigh[n] <- as.numeric(rayleigh_res$p.value)

                # # Hermans-Rasson test
                # hr_res <- HR_test(angle_thispop[ind_pregnant], iter = 1000)
                # T_HR[n] <- hr_res["Test statistic (T)"]
                # p_HR[n] <- hr_res["p-value"]
            } else {
                z_rayleigh[n] <- NA
                p_rayleigh[n] <- NA
                # T_HR[n] <- NA
                # p_HR[n] <- NA
            }
        }
        # # Rayleigh test summary
        # cat("Rayleigh test for", sp, "in", yr, "\n")
        # cat("z_rayleigh =", mean(z_rayleigh, na.rm = TRUE), "+/-", sd(z_rayleigh, na.rm = TRUE), "\n")
        # cat("z2.5% =", quantile(z_rayleigh, 0.025, na.rm = TRUE), "\n")
        # cat("z50% =", quantile(z_rayleigh, 0.5, na.rm = TRUE), "\n")
        # cat("z97.5% =", quantile(z_rayleigh, 0.975, na.rm = TRUE), "\n\n")

        # cat("p_rayleigh =", mean(p_rayleigh, na.rm = TRUE), "+/-", sd(p_rayleigh, na.rm = TRUE), "\n")
        # cat("2.5% =", quantile(p_rayleigh, 0.025, na.rm = TRUE), "\n")
        # cat("50% =", quantile(p_rayleigh, 0.5, na.rm = TRUE), "\n")
        # cat("97.5% =", quantile(p_rayleigh, 0.975, na.rm = TRUE), "\n")
        # cat("Q005 =", mean(p_rayleigh <= 0.05, na.rm = TRUE), "\n\n")

        # save these in a dataframe
        colnum <- which(statdf$Species_name == sp & statdf$year == yr)

        statdf$circmean[colnum] <- mean(mean_angle, na.rm = TRUE)
        statdf$circmean_sd[colnum] <- sd(mean_angle, na.rm = TRUE)
        statdf$z_rayleigh[colnum] <- mean(z_rayleigh, na.rm = TRUE)
        statdf$z_rayleigh_sd[colnum] <- sd(z_rayleigh, na.rm = TRUE)
        statdf$p_rayleigh[colnum] <- mean(p_rayleigh, na.rm = TRUE)
        statdf$z2.5[colnum] <- quantile(z_rayleigh, 0.025, na.rm = TRUE)
        statdf$z50[colnum] <- quantile(z_rayleigh, 0.5, na.rm = TRUE)
        statdf$z97.5[colnum] <- quantile(z_rayleigh, 0.975, na.rm = TRUE)
        statdf$p2.5[colnum] <- quantile(p_rayleigh, 0.025, na.rm = TRUE)
        statdf$p50[colnum] <- quantile(p_rayleigh, 0.5, na.rm = TRUE)
        statdf$p97.5[colnum] <- quantile(p_rayleigh, 0.975, na.rm = TRUE)
        statdf$Q005[colnum] <- mean(p_rayleigh <= 0.05, na.rm = TRUE)
    }
}

View(statdf)

# # save the results
write.csv(statdf, "models/circular/rayleigh_test_results_onset.csv", row.names = FALSE)

statdf <- read.csv("models/circular/rayleigh_test_results_onset.csv")
statdf$spname <- ifelse(statdf$Species_name == "Artocarpus heterophyllus", "jackfruit",
    ifelse(statdf$Species_name == "Mangifera indica", "mango", "tamarind")
)
# plot this
sw_circ_onsetplot <- ggplot(statdf %>% filter(year != 2013), aes(x = circmean, , y = z_rayleigh, fill = spname)) +
    # geom_histogram() +
    geom_bar(stat = "identity", position = "dodge", width = 2) +
    coord_polar() +
    geom_segment(aes(y = 0, xend = circmean, yend = z_rayleigh), arrow = arrow(length = unit(0.3, "cm"))) +
    facet_wrap(~spname) +
    scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    # tag_facets() +
    labs(
        x = "",
        y = "Rayleigh z statistic",
        title = "Circular mean of flowering onset"
    ) +
    scale_x_continuous(
        breaks = seq(0, 330, by = 30),
        # labels = c("June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
        labels = c("July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June")
    ) +
    # add a line for the mean
    # geom_vline(aes(xintercept = circ.mean.allyrs), linetype = "dashed", color = "black") +
    guides(fill = "none") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12))

png("doc/display/circ_mean_boot_onset.png", width = 12, height = 6, units = "in", res = 300)
sw_circ_onsetplot
dev.off()

library(patchwork)

layout <- "
A
B
"

png("doc/display/Fig2_new.png", width = 8, height = 8, units = "in", res = 300)
sw_circ_plot + sw_circ_onsetplot + plot_annotation(tag_levels = c("a", "b")) +
    plot_layout(design = layout)
dev.off()

View(statdf)

# mean flowering and onset across years----------------------------------

# expand grid for sps and years
statdf <- expand.grid(Species_name = sps)
# make empty columns for the results
statdf$circmean <- NA
statdf$circmean_sd <- NA
statdf$z_rayleigh <- NA
statdf_z_rayleigh_sd <- NA
statdf$p_rayleigh <- NA
statdf$z2.5 <- NA
statdf$z50 <- NA
statdf$z97.5 <- NA
statdf$p2.5 <- NA
statdf$p50 <- NA
statdf$p97.5 <- NA
statdf$Q005 <- NA


for (sp in sps) {
    fl.df2 <- fl.df %>% filter(Species_name == sp)

    week <- round((fl.df2$week_deg2), 2)
    fl <- fl.df2$fl_binary

    # Count individuals per week
    N_weeks <- as.numeric(table(week))
    week_unique <- round(as.numeric(names(table(week))), 2)
    N_min <- min(N_weeks)
    N_avg <- round(mean(N_weeks))

    N_count <- ifelse(option == "average", N_avg, N_min)

    set.seed(123)
    p_rayleigh <- numeric(N_populations)
    z_rayleigh <- numeric(N_populations)
    # p_HR <- numeric(N_populations)
    # T_HR <- numeric(N_populations)

    for (n in 1:N_populations) {
        week_thispop <- integer(0)
        fl_thispop <- integer(0)

        for (this_week in week_unique) {
            ind_week <- which(week == this_week)
            ind_week_random <- sample(ind_week, size = N_count, replace = TRUE)
            week_thispop <- c(week_thispop, week[ind_week_random])
            fl_thispop <- c(fl_thispop, fl[ind_week_random])
        }

        # Convert weeks to circular
        angle_thispop <- circular((week_thispop), type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")
        ind_fl <- which(fl_thispop == 1)

        # Rayleigh test
        if (length(ind_fl) > 0) {
            # circular mean
            mean_angle <- circular::mean.circular(angle_thispop[ind_fl], na.rm = TRUE)
            rayleigh_res <- rayleigh.test(angle_thispop[ind_fl])
            z_rayleigh[n] <- as.numeric(rayleigh_res$statistic)
            p_rayleigh[n] <- as.numeric(rayleigh_res$p.value)

            # # Hermans-Rasson test
            # hr_res <- HR_test(angle_thispop[ind_pregnant], iter = 1000)
            # T_HR[n] <- hr_res["Test statistic (T)"]
            # p_HR[n] <- hr_res["p-value"]
        } else {
            z_rayleigh[n] <- NA
            p_rayleigh[n] <- NA
            # T_HR[n] <- NA
            # p_HR[n] <- NA
        }
    }

    # save these in a dataframe
    colnum <- which(statdf$Species_name == sp)

    statdf$circmean[colnum] <- mean(mean_angle, na.rm = TRUE)
    statdf$circmean_sd[colnum] <- sd(mean_angle, na.rm = TRUE)
    statdf$z_rayleigh[colnum] <- mean(z_rayleigh, na.rm = TRUE)
    statdf$z_rayleigh_sd[colnum] <- sd(z_rayleigh, na.rm = TRUE)
    statdf$p_rayleigh[colnum] <- mean(p_rayleigh, na.rm = TRUE)
    statdf$z2.5[colnum] <- quantile(z_rayleigh, 0.025, na.rm = TRUE)
    statdf$z50[colnum] <- quantile(z_rayleigh, 0.5, na.rm = TRUE)
    statdf$z97.5[colnum] <- quantile(z_rayleigh, 0.975, na.rm = TRUE)
    statdf$p2.5[colnum] <- quantile(p_rayleigh, 0.025, na.rm = TRUE)
    statdf$p50[colnum] <- quantile(p_rayleigh, 0.5, na.rm = TRUE)
    statdf$p97.5[colnum] <- quantile(p_rayleigh, 0.975, na.rm = TRUE)
    statdf$Q005[colnum] <- mean(p_rayleigh <= 0.05, na.rm = TRUE)
}

View(statdf)

# # save the results
write.csv(statdf, "models/circular/rayleigh_test_results_allyr.csv", row.names = FALSE)


# onset

# expand grid for sps and years
statdf <- expand.grid(Species_name = sps)
# make empty columns for the results
statdf$circmean <- NA
statdf$circmean_sd <- NA
statdf$z_rayleigh <- NA
statdf_z_rayleigh_sd <- NA
statdf$p_rayleigh <- NA
statdf$z2.5 <- NA
statdf$z50 <- NA
statdf$z97.5 <- NA
statdf$p2.5 <- NA
statdf$p50 <- NA
statdf$p97.5 <- NA
statdf$Q005 <- NA


for (sp in sps) {
    fl.df2 <- flon.df %>% filter(Species_name == sp)

    week <- round((fl.df2$week_deg2), 2)
    fl <- fl.df2$fl_binary

    # Count individuals per week
    N_weeks <- as.numeric(table(week))
    week_unique <- round(as.numeric(names(table(week))), 2)
    N_min <- min(N_weeks)
    N_avg <- round(mean(N_weeks))

    N_count <- ifelse(option == "average", N_avg, N_min)

    set.seed(123)
    p_rayleigh <- numeric(N_populations)
    z_rayleigh <- numeric(N_populations)
    # p_HR <- numeric(N_populations)
    # T_HR <- numeric(N_populations)

    for (n in 1:N_populations) {
        week_thispop <- integer(0)
        fl_thispop <- integer(0)

        for (this_week in week_unique) {
            ind_week <- which(week == this_week)
            ind_week_random <- sample(ind_week, size = N_count, replace = TRUE)
            week_thispop <- c(week_thispop, week[ind_week_random])
            fl_thispop <- c(fl_thispop, fl[ind_week_random])
        }

        # Convert weeks to circular
        angle_thispop <- circular((week_thispop), type = "angles", units = "degrees", template = "clock12", modulo = "2pi", zero = 0, rotation = "clock")
        ind_fl <- which(fl_thispop == 1)

        # Rayleigh test
        if (length(ind_fl) > 0) {
            # circular mean
            mean_angle <- circular::mean.circular(angle_thispop[ind_fl], na.rm = TRUE)
            rayleigh_res <- rayleigh.test(angle_thispop[ind_fl])
            z_rayleigh[n] <- as.numeric(rayleigh_res$statistic)
            p_rayleigh[n] <- as.numeric(rayleigh_res$p.value)

            # # Hermans-Rasson test
            # hr_res <- HR_test(angle_thispop[ind_pregnant], iter = 1000)
            # T_HR[n] <- hr_res["Test statistic (T)"]
            # p_HR[n] <- hr_res["p-value"]
        } else {
            z_rayleigh[n] <- NA
            p_rayleigh[n] <- NA
            # T_HR[n] <- NA
            # p_HR[n] <- NA
        }
    }

    # save these in a dataframe
    colnum <- which(statdf$Species_name == sp)

    statdf$circmean[colnum] <- mean(mean_angle, na.rm = TRUE)
    statdf$circmean_sd[colnum] <- sd(mean_angle, na.rm = TRUE)
    statdf$z_rayleigh[colnum] <- mean(z_rayleigh, na.rm = TRUE)
    statdf$z_rayleigh_sd[colnum] <- sd(z_rayleigh, na.rm = TRUE)
    statdf$p_rayleigh[colnum] <- mean(p_rayleigh, na.rm = TRUE)
    statdf$z2.5[colnum] <- quantile(z_rayleigh, 0.025, na.rm = TRUE)
    statdf$z50[colnum] <- quantile(z_rayleigh, 0.5, na.rm = TRUE)
    statdf$z97.5[colnum] <- quantile(z_rayleigh, 0.975, na.rm = TRUE)
    statdf$p2.5[colnum] <- quantile(p_rayleigh, 0.025, na.rm = TRUE)
    statdf$p50[colnum] <- quantile(p_rayleigh, 0.5, na.rm = TRUE)
    statdf$p97.5[colnum] <- quantile(p_rayleigh, 0.975, na.rm = TRUE)
    statdf$Q005[colnum] <- mean(p_rayleigh <= 0.05, na.rm = TRUE)
}

# save the results
write.csv(statdf, "models/circular/rayleigh_test_results_onset_allyrs.csv", row.names = FALSE)


# year to year variation in tree level flowering onset------------------

# are there years when a tree flowered twice?
n.flon <- flon.df %>%
    filter(fl_binary == 1) %>%
    group_by(Species_name, User_Tree_id, year2) %>%
    dplyr::summarise(
        n.fl = length(fl_binary)
    ) %>%
    arrange(desc(n.fl))

# pick trees year combinations with only 1 flowering event
n.flon1 <- n.flon %>%
    filter(n.fl == 1) %>%
    dplyr::mutate(sp_tree_yr = paste0(Species_name, "_", User_Tree_id, "_", year2)) %>%
    pull(sp_tree_yr)


tree_flon <- flon.df %>%
    filter(fl_binary == 1, year2 != 2022) %>%
    # # select only trees with 1 flowering event
    # filter(paste0(Species_name, "_", User_Tree_id, "_", year2) %in% n.flon1) %>%
    # for each tree x year, pick the first flowering onset event
    group_by(Species_name, User_Tree_id, year2) %>%
    dplyr::summarise(week_deg2 = min(week_deg2)) %>%
    ungroup() %>%
    group_by(Species_name, User_Tree_id) %>%
    dplyr::summarise(
        flon_week = mean(week_deg2, na.rm = T),
        flon_week_sd = sd(week_deg2, na.rm = T),
        n.fl = length(week_deg2)
    ) %>%
    ungroup() %>%
    dplyr::mutate(spname = ifelse(Species_name == "Artocarpus heterophyllus", "jackfruit",
        ifelse(Species_name == "Mangifera indica", "mango", "tamarind")
    )) # %>%
# remove trees with <3 observations
# filter(n.fl >= 3)


head(tree_flon)
table(tree_flon$spname[tree_flon$n.fl >= 2])

# plot distribution of means
tree_flon_plot <- ggplot(tree_flon, aes(x = flon_week, fill = spname)) +
    geom_histogram(bins = 20) +
    facet_wrap(~spname) +
    scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    labs(
        y = "Number of trees",
        x = "Mean flowering week",
        title = "Distribution of tree-level mean flowering onset week"
    ) +
    coord_polar() +
    scale_x_continuous(
        breaks = seq(0, 330, by = 30),
        # labels = c("June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
        labels = c("July", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June")
    ) +
    guides(fill = "none") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12))

# distribution of sds
tree_flon_sd_plot <- ggplot(tree_flon %>% filter(n.fl >= 3), aes(x = (flon_week_sd * 52) / 360, fill = spname)) +
    geom_histogram(bins = 20) +
    facet_wrap(~spname) +
    scale_color_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    scale_fill_manual(values = c("#1e824c", "#f7ca18", "#a6915c")) +
    labs(
        x = "Standard deviation (weeks) of tree-level flowering onset across years",
        y = "Number of trees"
    ) +
    # add a line for the median by species
    geom_vline(data = tree_flon %>% filter(n.fl >= 3) %>% group_by(spname) %>% dplyr::summarise(medwk = median(flon_week_sd * (52 / 360), na.rm = T)), aes(xintercept = medwk), linetype = "dashed", color = "black") +
    geom_text(data = tree_flon %>% filter(n.fl >= 3) %>% group_by(spname) %>% dplyr::summarise(medwk = median(flon_week_sd * (52 / 360), na.rm = T)), aes(x = medwk, y = 25, label = paste0("median = ", round(medwk, 2)))) +
    # coord_polar() +
    guides(fill = "none") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12))

library(patchwork)
png("doc/display/Fig3_new.png", width = 8, height = 8, units = "in", res = 300)
tree_flon_plot + tree_flon_sd_plot + plot_annotation(tag_levels = c("a", "b")) +
    plot_layout(design = layout)
dev.off()

write.csv(tree_flon, "models/circular/tree_flowering_onset.csv", row.names = FALSE)

# look at trees with more than 1 flowering event
n.flon2 <- n.flon %>%
    filter(n.fl > 1) %>%
    dplyr::mutate(sp_tree_yr = paste0(Species_name, "_", User_Tree_id, "_", year2)) %>%
    pull(sp_tree_yr)

tree_flon_mult <- flon.df %>%
    filter(fl_binary == 1, year2 != 2022) %>%
    # select only trees with 1 flowering event
    filter(paste0(Species_name, "_", User_Tree_id, "_", year2) %in% n.flon2)

View(tree_flon_mult)
