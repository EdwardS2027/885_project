



# Setting up the layout for three histograms in one figure
par(mfrow = c(1, 2)) # Divide the plotting area into 1 row and 3 columns


# Histogram for Alive (vital = 1)
hist(data2$events[data2$vital == 1],
     breaks = 30, 
     main = "Alive",
     xlab = "Number of Desaturation Events",
     col = "lightgreen",
     border = "black")

# Histogram for Dead (vital = 0)
hist(data2$events[data2$vital == 0],
     breaks = 30, 
     main = "Dead",
     xlab = "Number of Desaturation Events",
     col = "salmon",
     border = "black")

# Reset layout
par(mfrow = c(1, 1))




hist(data2$events[data2$vital == 1],
     breaks = 30, 
     main = "Desaturation Events by Vital Status",
     xlab = "Number of Desaturation Events",
     col = rgb(0.2, 0.8, 0.2, 0.5), # Semi-transparent green for Alive
     border = "black",
     xlim = range(data2$events),
     ylim = c(0, max(hist(data2$events[data2$vital == 1], breaks = 30, plot = FALSE)$counts,
                     hist(data2$events[data2$vital == 0], breaks = 30, plot = FALSE)$counts)),
     freq = TRUE, # Ensure frequency scale
     cex.main = 1.5,  # Increase title size
     cex.lab = 1.4,   # Increase axis label size
     cex.axis = 1.2   # Increase axis tick size
)

# Add the Dead histogram
hist(data2$events[data2$vital == 0],
     breaks = 30, 
     col = rgb(1, 0.4, 0.4, 0.5), # Semi-transparent red for Dead
     border = "black",
     add = TRUE) # Overlay on the existing plot

# Add a legend with larger text
legend("topright", 
       legend = c("Alive", "Dead"), 
       fill = c(rgb(0.2, 0.8, 0.2, 0.5), rgb(1, 0.4, 0.4, 0.5)),
       border = "black", 
       cex = 1.5) 








# Setting up the layout for three histograms in one figure
par(mfrow = c(1, 2)) # Divide the plotting area into 1 row and 3 columns


# Histogram for Alive (vital = 1)
hist(data3$desat_slope[data3$vital == 1],
     breaks = 30, 
     main = "Alive",
     xlab = "Desaturation Slope",
     col = "lightgreen",
     border = "black")

# Histogram for Dead (vital = 0)
hist(data3$desat_slope[data3$vital == 0],
     breaks = 30, 
     main = "Dead",
     xlab = "Desaturation Slope",
     col = "salmon",
     border = "black")

# Reset layout
par(mfrow = c(1, 1))





# 4% Minimum Depth 5 Seconds Minimum Duration
data3 = seven[complete.cases(seven$desat_slope),c(2,16)]
data3 <- data3 %>%
  arrange(pptid, desat_slope) %>%
  group_by(pptid) %>%
  mutate(event = row_number()) %>%
  ungroup()

data3=as.data.frame(data3)

fourpfives = data.frame(pptid = seven[,2], "events" = NA,"desat_slope" = NA)
fourpfives <- fourpfives[order(fourpfives$pptid), ]
seven=as.data.frame(seven)
# for each subject
for (i in 1:length(unique(seven[,2]))) 
{
  # filter out specfic id we are interested in
  id = unique(seven[,2])[i]
  
  
  # number of desaturation events
  filter = seven[which(seven[,2]==id),]
  
  filter2 = subset(filter, !(waso >= 2 & stage == "W"))
  #filter <- filter[!is.na(filter$desat_slope), ]
  # it takes only the valid desaturated events - NA means no desaturated event
  # filter NA before for computational time
  # maybe 
  filter2 = filter2[which(!is.na(filter2$desat_start_ind)),]
  for(j in 1:length())
  {
    
  }
  event = nrow(filter2)
  
  
  # total sleep time
  #filter out the wake stages and times by 30 seconds - since each is 30 second interval
  tst = nrow(filter[which(filter$stage !="W"),])*30
  # convert into minute
  tst = tst/60
  #convert into hour
  tst = tst/60
  
  # calculate for ODI
  fourpfives[which(fourpfives$pptid ==id),2] = event
  fourpfives[which(fourpfives$pptid ==id),3] = event/tst
  fourpfives[which(fourpfives$pptid ==id),4] = median(filter2$desat_slope,na.rm =TRUE)
  fourpfives[which(fourpfives$pptid ==id),5] = median(filter2$desat_dur,na.rm =TRUE)
  # track
  print(i)
}





data4 <- data3 %>%
  left_join(death[,c(1,3)],by="pptid")

library(ggplot2)


median_data <- data4 %>%
  group_by(event) %>%
  summarise(median_desat_slope = median(desat_slope, na.rm = TRUE))

# Create the line plot using ggplot2
ggplot(median_data, aes(x = event, y = median_desat_slope)) +
  geom_line() +                   # Line plot
  geom_point() +                  # Points at each event
  labs(x = "Event", 
       y = "Median of desat_slope", 
       title = "Median desat_slope vs Event") +
  theme_minimal()       
data4 %>%
  # Group by vital and event, then calculate the median desat_slope
  group_by(vital, event) %>%
  summarise(median_slope = median(desat_slope, na.rm = TRUE)) %>%
  ggplot(aes(x = event, y = median_slope, group = vital, color = factor(vital))) +
  geom_line() + 
  geom_point() + 
  labs(x = "Event", y = "Median Desat Slope", color = "Vital Status") + 
  theme_minimal() +
  facet_wrap(~ vital, scales = "free_y") # Creates separate plots for vital = 0 and vital = 1



data4 %>%
  # Group by vital and event, then calculate the median desat_slope
  group_by(vital, event) %>%
  summarise(median_slope = median(desat_slope, na.rm = TRUE)) %>%
  ggplot(aes(x = event, y = median_slope, group = vital, color = factor(vital))) +
  geom_line() + 
  geom_point() + 
  labs(x = "Event Place", y = "Median Desaturation Slope", color = "Vital Status") + 
  theme_minimal() +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),  # Set custom colors for the lines
    labels = c("0" = "Dead", "1" = "Alive")  # Change legend labels to "Alive" and "Dead"
  ) +
  theme(
    axis.title = element_text(size = 16),  # Increase axis title size
    axis.text = element_text(size = 14),   # Increase axis tick label size
    legend.title = element_text(size = 16), # Increase legend title size
    legend.text = element_text(size = 14)   # Increase legend text size
  )






hist(data3$desat_slope[data3$vital == 1],
     breaks = 30, 
     main = "Desaturation Slope by Vital Status",
     xlab = "Desaturation Slope",
     col = rgb(0.2, 0.8, 0.2, 0.5), # Semi-transparent green for Alive
     border = "black",
     xlim = range(data3$desat_slope), # Set x-axis range to fit both histograms
     ylim = c(0, max(hist(data3$desat_slope[data3$vital == 1], breaks = 30, plot = FALSE)$counts,
                     hist(data3$desat_slope[data3$vital == 0], breaks = 30, plot = FALSE)$counts)), # Set y-axis range
     freq = TRUE) # Use frequency instead of density

# Add the Dead histogram
hist(data3$desat_slope[data3$vital == 0],
     breaks = 30, 
     col = rgb(1, 0.4, 0.4, 0.5), # Semi-transparent red for Dead
     border = "black",
     add = TRUE) # Overlay on the existing plot

# Add a legend
legend("topleft", # Change the position to the top-left corner
       legend = c("Alive", "Dead"), 
       fill = c(rgb(0.2, 0.8, 0.2, 0.5), rgb(1, 0.4, 0.4, 0.5)),
       border = "black")
