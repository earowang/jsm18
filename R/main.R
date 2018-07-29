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
library(xkcd)

## ---- sx
sx <- rwalkr::run_melb(year = 2017, sensor = "Southern Cross Station", tz = "Australia/Melbourne") %>%
  select(-Sensor)
time_breaks <- seq.int(0, 23, by = 4)

## ---- animate
sx %>%
  ggplot(aes(x = Date_Time, y = Count)) +
  geom_line()

jan9 <- sx %>%
  filter(Date == as.Date("2017-01-09"))

jan9_animate <- map_dfr(tsibble::stretcher(jan9, .combine = TRUE),
  ~ mutate(., id = n())
)
xrange <- range(jan9_animate$Time)
yrange <- range(jan9_animate$Count)
ratioxy <- diff(xrange) / diff(yrange)

mapping <- aes(
  x=x,
  y=y,
  group = id,
  scale=scale,
  ratioxy=ratioxy,
  angleofspine = angleofspine,
  anglerighthumerus = anglerighthumerus,
  anglelefthumerus = anglelefthumerus,
  anglerightradius = anglerightradius,
  angleleftradius = angleleftradius,
  anglerightleg =  anglerightleg,
  angleleftleg = angleleftleg,
  angleofneck = angleofneck
)

dataman <- data.frame(
  x= jan9$Time,
  y= jan9$Count + 450,
  id = 1:24,
  scale = 200,
  ratioxy = ratioxy,
  angleofspine = -pi / 2,
  anglerighthumerus = -pi / 6,
  anglelefthumerus = pi + pi / 6,
  anglerightradius = 0,
  angleleftradius = -pi / 4,
  angleleftleg = 3 * pi / 2  + pi / 12 ,
  anglerightleg = 3 * pi / 2  - pi / 12,
  angleofneck = 3 * pi / 2 - pi / 10
  )
p1 <- jan9_animate %>%
  ggplot(aes(x = Time, y = Count)) +
  geom_line(aes(group = id), size = 2) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_breaks,
    limits = c(0, 23)
  ) +
  xkcdman(mapping, dataman) +
  xlab("Time") +
  theme_remark() +
  theme_xkcd() +
  transition_manual(id)
animate(p1, 100, 2, width = 1000, height = 700)

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
sx_jan <- sx %>%
  filter(Date < as.Date("2017-02-01"))
p_sx_jan <- sx_jan %>%
  frame_calendar(x = Time, y = Count, date = Date) %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
prettify(p_sx_jan, label = c("label", "text", "text2"), size = 7)

## ---- sx-1
sx_cal <- sx %>%
  frame_calendar(x = Time, y = Count, date = Date)
sx_cal

## ---- sx-2
p_sx <- sx_cal %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
p_sx

## ---- sx-2b
p_sx <- ggplot(sx_cal, aes(.Time, .Count, group = Date)) +
  geom_line()

## ---- sx-3
prettify(p_sx)

## ---- sx-plotly
library(plotly)
pp_sx <- sx_cal %>%
  group_by(Date) %>%
  plot_ly(x = ~ .Time, y = ~ .Count) %>%
  add_lines()
prettify(pp_sx)

## ---- weekly
sx13 <- sx %>%
  filter(Date < as.Date("2017-04-01"))
p2 <- sx13 %>%
  frame_calendar(x = Time, y = Count, date = Date, calendar = "weekly") %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
prettify(p2, size = 7)

## ---- daily
p3 <- sx13 %>%
  frame_calendar(x = Time, y = Count, date = Date, calendar = "daily") %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
prettify(p3, size = 7)

## ---- linear
pl <- sx13 %>%
  frame_calendar(x = Time, y = Count, date = Date) %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_line() +
  theme_remark()
prettify(pl, size = 7)

## ---- polar
pp <- sx13 %>%
  frame_calendar(x = Time, y = Count, date = Date, polar = TRUE) %>%
  ggplot(aes(.Time, .Count, group = Date)) +
  geom_path() +
  theme_remark()
prettify(pp, size = 7)

## ---- multiple
ped <- rwalkr::run_melb(
  year = 2017,
  sensor = c("Southern Cross Station", "Birrarung Marr"),
  tz = "Australia/Melbourne"
)

p_m <- ped %>%
  group_by(Sensor) %>%
  frame_calendar(x = Time, y = Count, date = Date) %>%
  ggplot(aes(x = .Time, y = .Count, group = Date, colour = Sensor)) +
  geom_line() +
  facet_wrap(Sensor ~ .) +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_m, label.padding = unit(0.1, "lines"))

## ---- chn
prettify(p_sx_jan, locale = "zh", family = "STKaiti", size = 7)

## ---- fr
prettify(p_sx_jan, locale = "fr", size = 7)
