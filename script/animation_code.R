# Thu Jun 18 10:57:04 2020 ------------------------------

# setup -------------------------------------------------------------------
# make list of required packages
list.of.packages <- c("tidyverse",
                      "lubridate",
                      "maps", 
                      "rgeos",
                      "maptools",
                      "ggmap",
                      "gganimate", 
                      "sf", 
                      "rgdal"
)

# make list of packages that are required, but not already downloaded
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# download packages that are not already present in the library
if(length(new.packages))
  install.packages(new.packages)

# load packages
packages_load <- lapply(list.of.packages, require, character.only = T)

# print warning if there is a problem with installing/loading some of packages
if(any(as.numeric(packages_load)==0)){
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != T], sep = ","), "not loaded!"))
}else{
  print("All packages were successfully loaded.")
}


# get data ----------------------------------------------------------------
# set up temporary file
tempdl <- tempfile()

# download zipped data file from BARD website
download.file('http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/resources/csvs/UCD_GS_RESCUE.zip', 
              tempdl, 
              mode = 'wb')

# unzip into data folder
unzip(tempdl, "UCD_GS_RESCUE.csv", exdir = "data")

# read csv into work space (also reducing the # of TagIDs read to simplify)
gs_dat <- read_csv_chunked('data/UCD_GS_RESCUE.csv', callback = DataFrameCallback$new(function(x, pos) subset(x, TagID>46635)),  progress = T)

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

CA_map<- map_data('state', region = 'california') # get outline of CA

CA<-c(geom_polygon(aes(long,lat,group=group), 
                   size = 0.1, 
                   colour= "#090D2A",#090D2A", 
                   fill="#090D2A", alpha=0.8, data=CA_map))

# GS46638 <- GS46638 %>% 
#   mutate(dt = as.character(DetectDate), id = 1:n())
# 
# 
# plot <- ggplot() +
#   CA +
#   geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") + 
#   geom_point(data = GS46638, aes(x = Lon, y = Lat), color = "red", size = 3.5) + 
#   annotate("text", label = paste("TagID:",GS46638$TagID[1]), color =  "white", x = -125, y = 37.1, size = 6) +
#   theme(panel.background =  element_rect(fill='gray40',colour='gray40'),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) + 
#   coord_cartesian(xlim = c(-125.5, -119), ylim = c(37,41)) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank()) + 
#   labs(title = '{GS46638$DetectionLocation[as.integer(closest_state)]}', 
#        subtitle = '{GS46638$DetectDate[as.integer(closest_state)]}') + 
#   transition_states(id, transition_length = .1, state_length = 1)


plot <- ggplot() +
  CA +                                                                               # ad CA outline to plot
  geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") +            # add receiver array to plot
  geom_point(data = GS46638, aes(x = Lon, y = Lat), color = "red", size = 3.5) + 
  annotate("text", label = paste("TagID:",GS46638$TagID[1]),                         # add text label to plot
           color =  "white", x = -125, y = 37.1, size = 6) +
  theme(panel.background =  element_rect(fill='gray40',colour='gray40'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(xlim = c(-125.5, -119), ylim = c(37,41)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  transition_manual(frames = DetectDate) + 
  labs(title = 'Location: {GS46638$DetectionLocation[current_frame]}',
       subtitle = 'Date: {current_frame}')

# animate plot
animate(plot, fps=2)

# create fig_output directory
dir.create("fig_output")

# save animation to directory
anim_save("fig_output/GS46638.gif", plot)

# optional: save as mp4 to pause and click through frame by frame
b <- animate(plot, renderer = av_renderer())
anim_save("fig_output/GS4638.mp4", b)




# Faceted plot ------------------------------------------------------------
# plot 4 fish at once, keep time relative to explore differences in migration timing
# subset data by time  - keep post rescue detections; not entire detection history
rescue_year <- dets %>% 
  filter(DetectDate<= "2012-01-01 00:00:00")



plot2 <- ggplot() +
  CA +                                                                               # ad CA outline to plot
  geom_point(data = recs_short, aes(x = lon, y = lat), color = "white") +            # add receiver array to plot
  geom_point(data = post_rescue, aes(x = Lon, y = Lat, color = factor(TagID)),
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

# animate plot
animate(plot2, fps=2, renderer = gifski_renderer(loop = F))