source("R/theme.R")

## ---- custom
gif2mp4 <- function(gif, path){
  tmp_gif <- tempfile()
  magick::image_write(gif, tmp_gif)
  message("Converting gif to mp4...")
  system(
    paste0('ffmpeg -i ', tmp_gif, ' -y -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" ',
    file.path(getwd(), normalizePath(path)))
  )
  message("Done!")
}

## ---- load
library(tidyverse)
library(gganimate)

## ---- sx
sx <- rwalkr::run_melb(year = 2017, sensor = "Southern Cross Station", tz = "Australia/Melbourne")
time_breaks <- seq.int(0, 23, by = 4)

## ---- animate
sx %>%
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line()

jan9 <- sx %>%
  filter(Date == as.Date("2017-01-09"))

jan9_animate <- map_dfr(tsibble::stretcher(jan9, .flatten = TRUE),
  ~ mutate(., id = n())
)

p1 <- jan9_animate %>%
  ggplot(aes(x = Time, y = Count)) +
  geom_line(aes(group = id), .size = 2) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_breaks,
    limits = c(0, 23)
  ) +
  xlab("Time") +
  theme_remark() +
  transition_manual(id)
animate(p1, 100, 2, width = 1000, height = 800)

rng_x1 <- range(jan9$Date_Time, na.rm = TRUE)
rng_x2 <- range(
  sx %>%
    filter(Date >= as.Date("2017-01-09"), Date < as.Date("2017-01-16")) %>%
    pull(Date_Time), na.rm = TRUE
)
rng_x3 <- range(
  sx %>%
    filter(Date >= as.Date("2017-01-01"), Date < as.Date("2017-02-01")) %>%
    pull(Date_Time), na.rm = TRUE
)
rng_x4 <- range(
  sx %>%
    filter(Date >= as.Date("2017-01-01"), Date < as.Date("2017-07-01")) %>%
    pull(Date_Time), na.rm = TRUE
)
rng_x <- range(sx$Date_Time, na.rm = TRUE)
rng_y <- range(sx$Count, na.rm = TRUE)
p2 <- sx %>%
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line() +
  xlab("Date Time") +
  theme_remark() +
  view_step_manual(
    5, 2,
    c(rng_x1[1], rng_x1[1], rng_x2[1], rng_x3[1], rng_x4[1], rng_x[1]),
    c(rng_x1[2], rng_x1[2], rng_x2[2], rng_x3[2], rng_x4[2], rng_x[2]),
    rng_y[1],
    rng_y[2],
    fixed_y = TRUE,
    wrap = FALSE
  )
gif2 <- animate(p2, 100, 5, width = 1000, height = 600)
gif2 %>%
  gif2mp4("img/sx17.mp4")

## ---- sensor-map
library(leaflet)
sensor_loc <- rwalkr::pull_sensor()
leaflet(sensor_loc) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude, color = I("#542788"),
    label = ~ Sensor, fillOpacity = 0.7
  )

## ---- sx-wrap
library(lubridate)
sx %>% 
  mutate(wday = wday(Date, label = TRUE, week_start = 1)) %>% 
  ggplot(aes(x = Time, y = Count, group = Date)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ wday, ncol = 4) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_breaks,
    limits = c(0, 23)
  ) +
  xlab("Time") +
  theme_remark()

## ---- sx-hol
library(sugrrants)
library(tsibble)
sx_cal <- sx %>% 
  mutate(Holiday = if_else(Date %in% c(holiday_aus(2017)$date, as_date("2017-09-29")), 
    TRUE, FALSE)) %>% 
  frame_calendar(x = Time, y = Count, date = Date)
p_sx <- sx_cal %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Holiday)) +
  geom_line() +
  theme_remark()
prettify(p_sx)

## ---- sx-oct
sx_oct <- sx %>% 
  filter(Date < as.Date("2017-02-01")) %>% 
  frame_calendar(x = Time, y = Count, date = Date) %>% 
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
prettify(sx_oct, label = c("label", "text", "text2"))
