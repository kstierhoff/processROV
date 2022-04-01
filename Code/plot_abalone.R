LOG.plot <- LOG %>% 
  mutate(date_time = mdy_hms(date_time))

abalone <- filter(LOG.plot, comment %in% c("H. kamtschatkana","H. sorenseni"))

ggplot(LOG.plot, aes(date_time, event_id)) + geom_line() + 
  geom_point(data = abalone, aes(date_time, event_id, fill = comment), shape = 21)

write_csv(abalone, here("Output/abalone_sightings.csv"))

ggplot() + 
  geom_path(data = DAT, aes(long_r, lat_r, group = dive_name), show.legend = FALSE) + 
  geom_point(data = abalone, aes(long_r, lat_r, 
                                 fill = comment), 
             size = 3, shape = 21) +
  coord_map()
