library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(logger)
library(lubridate)

# Config packages
log_threshold(DEBUG)
theme_set(theme_bw(14))

#### CONSTANTS ####
URL <- "https://paquita.masto.host/users/paquita_stats/outbox?page=true"
DELAY_SECONDS <- 5

#### FUNCTIONS ####
get_page <- function(url) {
    dd <- fromJSON(url)
    log_info("Read {url}")
    log_info("Read {nrow(dd$orderedItems)} toots")
    toots <- dd$orderedItems[dd$orderedItems$type != "Announce", ]
    if (class(toots$object) == "data.frame") {
        toot_data <- data.frame(published = toots$published,
            content = toots$object$content)
    } else {
        toot_data <- data.frame(published = toots$published, 
            content = sapply(toots$object, function(x) x$content))
    }
    return(list(toots = toot_data, next_page = dd$`next`))
}

get_all_toots <- function(url) {
    next_page <- url
    toots <- data.frame()
    keep_reading <- TRUE
    while (keep_reading) {
        data <- get_page(next_page)
        next_page <- data$next_page
        toots <- bind_rows(toots, data$toots)
        keep_reading <- !is.null(data$next_page)
        log_info("Progress so far: {nrow(toots)} toots, keep_reading = {keep_reading}")
        Sys.sleep(DELAY_SECONDS)
    }
    return(toots)
}

get_stats_from_toot <- function(toot_text) {
    numbers_in_toot <- sapply(regmatches(toot_text, gregexpr("[0-9]+", toot_text)),
        as.numeric)
    return(data.frame(usuarios_totales = numbers_in_toot[1],
        usuarios_activos = numbers_in_toot[2],
        publicaciones = numbers_in_toot[3],
        servidores = numbers_in_toot[4]))
}

parse_toots <- function(toot_df) {
    dates <- parse_date_time(toots$published, "%Y-%m-%d %H:%M:%S")
    stats <- bind_rows(lapply(toot_df$content, get_stats_from_toot))
    stats$dates <- dates
    # Add a few extra fields
    stats <- stats %>%
        arrange(dates) %>% 
        mutate(publicaciones_nuevas = publicaciones - lag(publicaciones, default = publicaciones[1]))
    return(stats)
}

#### MAIN ####
toots <- get_all_toots(URL)
stats <- parse_toots(toots)

plt1 <- ggplot(gather(stats, key, value, c(usuarios_totales, usuarios_activos))) + 
    geom_line(aes(x = dates, y = value, color = key)) +
    scale_color_brewer(palette = "Set1", name = NULL,
        labels = c("Usuarios activos", "Usuarios totales")) +
    scale_y_continuous(limits = c(0, NA)) +
    theme(legend.position = "top") +
    ylab("Número de usuarios") +
    xlab("Fecha") +
    ggtitle("Número de usuarios (totales y activos) en paquita.masto.host", 
        sub = "Un usuario activo es aquel que ha entrado en los últimos 30 días")
plot(plt1)
ggsave("/tmp/paquita_stats1.png", dpi = 300)

plt2 <- ggplot(stats) +
    geom_line(aes(x = dates, y = publicaciones)) +
    ylab("Toots") +
    xlab("Fecha") +
    ggtitle("Número de publicaciones")
plot(plt2)
ggsave("/tmp/paquita_stats2.png", dpi = 300)

plt3 <- ggplot(stats) +
    geom_line(aes(x = dates, y = publicaciones_nuevas / usuarios_activos)) +
    ylab("Toots / usuario activo") +
    xlab("Fecha") +
    ggtitle("Número de publicaciones por usuario activo")
plot(plt3)
ggsave("/tmp/paquita_stats3.png", dpi = 300)

plt4 <- ggplot(stats) +
    geom_line(aes(x = dates, y = publicaciones_nuevas)) +
    xlab("Fecha") +
    ggtitle("Nuevas publicaciones")
plot(plt4)
ggsave("/tmp/paquita_stats4.png", dpi = 300)