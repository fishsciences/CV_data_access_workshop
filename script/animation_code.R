# Thu Jun 18 10:57:04 2020 ------------------------------
source("script/utils.R")
# setup -------------------------------------------------------------------
check_and_install_packages()
download_telemetry_data()


# read csv into work space (also reducing the # of TagIDs read to simplify)
gs_dat <-
  read_csv_chunked(
    'data/UCD_GS_RESCUE.csv',
    callback = DataFrameCallback$new(function(x, pos)
      subset(x, TagID > 46635)),
    progress = TRUE
  )

## Prepare the data

# Recipe:
#   
#   * get sample data
#     + receiver locations
#     + subset single fish
#   * animate movement of single fish
#     + have all receivers on the map
#     + have detection "light up"
#     + don't scale time
#        - but have it displayed on the map for reference; long periods at sea for Adult GS
#   * save output as .gif?

# subset rec data by general location to make for a cleaner map (quick and dirty version)
recs_short <- gs_dat %>% 
  select(General_Location, Lat, Lon) %>% 
  group_by(General_Location) %>% 
  summarize(lat = mean(Lat), lon = mean(Lon))

# summarize detection data
dets <- gs_dat %>%
  select(TagID, DetectionLocation, DetectDate, Lat, Lon) %>%  #reduce the fields
  mutate(dt = as_date(DetectDate)) %>% # reduce the number of detections to one per day
  group_by(dt) %>% 
  filter(DetectDate == min (DetectDate)) %>% # take the first detection per hour for easier anmiations
  ungroup() %>%
  select(TagID, DetectionLocation, DetectDate, Lat, Lon)

# subset single fish
GS46638 <- dets %>% 
  filter(TagID==46638)


# Begin plotting ----------------------------------------------------------
# Now plot a single fish
# Following the tutorial found [here](https://medium.com/@mueller.johannes.j/use-r-and-gganimate-to-make-an-animated-map-of-european-students-and-their-year-abroad-517ad75dca06).

CA_map <-
  map_data('state', region = 'california') # get outline of CA

CA <- c(
  geom_polygon(
    aes(long, lat, group = group),
    size = 0.1,
    colour = "#090D2A",
    #090D2A",
    fill = "#090D2A",
    alpha = 0.8,
    data = CA_map
  )
)


plot <- ggplot() +
  CA +                                                                               # ad CA outline to plot
  geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") +            # add receiver array to plot
  geom_point(data = GS46638,
             aes(x = Lon, y = Lat),
             color = "red",
             size = 3.5) +
  annotate(
    "text",
    label = paste("TagID:", GS46638$TagID[1]),
    # add text label to plot
    color =  "white",
    x = -125,
    y = 37.1,
    size = 6
  ) +
  theme(
    panel.background =  element_rect(fill = 'gray40', colour = 'gray40'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-125.5,-119), ylim = c(37, 41)) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  transition_manual(frames = DetectDate) +
  labs(title = 'Location: {GS46638$DetectionLocation[current_frame]}',
       subtitle = 'Date: {current_frame}')



# create fig_output directory
dir.create("fig_output")

if(FALSE){ # none of the code between these braces worked to its intended end for me; see comments below. Don't have time to troubleshoot unfortunately, but consider simplifying

animate(plot, fps = 2) # animate plot - not run; creates a bunch of .pngs in root directory

# save animation to directory
anim_save("fig_output/GS46638.gif", plot, device = "png") # Error: The animation object does not specify a save_animation method            
}

# optional: save as mp4 to pause and click through frame by frame
b <- animate(plot, renderer = av_renderer()) # Error: The av package is required to use av_renderer; add to your package list or remove dependency here
anim_save("fig_output/GS46638.mp4", b)



# Faceted plot ------------------------------------------------------------
# plot 4 fish at once, keep time relative to explore differences in migration timing
# subset data by time  - keep post rescue detections; not entire detection history
rescue_year <- dets %>% 
  filter(DetectDate<= "2012-01-01 00:00:00")


plot2 <- ggplot() +
  CA +                                                                               # ad CA outline to plot
  geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") +            # add receiver array to plot
  geom_point(data = rescue_year, aes(x = Lon, y = Lat, color = factor(TagID)),
             size = 3.5, show.legend = F) + 
  #annotate("text", label = paste("TagID:",GS46638$TagID[1]),                         # add text label to plot
   #        color =  "white", x = -125, y = 37.1, size = 6) +
  theme(panel.background =  element_rect(fill='gray40',colour='gray40'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(xlim = c(-125.5, -119), ylim = c(37,41)) +
  facet_wrap(~TagID) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  transition_time(DetectDate) + 
  labs(title = "Date: {frame_time}")

if(FALSE) { # same animation issues as above
# animate plot
animate(plot2, fps=2, renderer = gifski_renderer(loop = F))
}
