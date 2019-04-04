### Create world heatmap, rural vs. urban 1x2 grid
facet_heatmap <- function(data, ind, yr) {
  
  # Create filtered datasets for each location
  data_urban <- select_ind_year_loc(data, ind, yr, "URBAN")
  data_rural <- select_ind_year_loc(data, ind, yr, "RURAL")
  
  # Joined filtered data with world map
  map_urban <- left_join(map_world, data_urban, by = 'country')
  map_rural <- left_join(map_world, data_rural, by = 'country')
  
  # Bind all data together
  map <- rbind(map_urban, map_rural)
  
  # Create data frame with facet property
  map <- data.frame(map, Facet = rep(c("map_urban","map_rural"),
                                     times=c(nrow(map_urban),nrow(map_rural))))
  
  # Text for plot title
  title <- map$seriesdescription %>% na.omit() %>% unique()
  
  # Plot graph
  ggplot(map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = value)) +
    
    # Labels and legend
    labs(title = "Population with access to electricity, by rural/urban",
         subtitle = paste('Year', yr),
         fill = '  %',
         x = NULL, y = NULL) +
    
    
    
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = 'lightgrey',
                        breaks = seq(0, 100, 20)) +
    
    # Theme adjustments
    theme(plot.title = element_text(size = 18, face = 'bold'),
          plot.subtitle = element_text(size = 16, hjust = 0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.key.size = unit(25, 'pt'),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(10,10,0,23),"mm")) + # top, right, bottom, left
    
    # Faceting
    facet_wrap(.~Facet)
  
}

### Create scatterplot, rural vs. urban 1x2 grid
facet_scatterplot <- function(data, ind, start, end, x_max = end) {
  
  # Create function that filters data used for plotting
  filter_data <- function(data, ind, start, end, loc) {
    
    dat %>%
      
      # Filter for indicator, given year interval and location
      filter(indicator == ind, year %in% c(start:end), location == loc) %>%
      
      # Add column for nr. of people covered (in millions) for all countries
      group_by(year) %>%
      mutate(total_people_million = sum(nr_people/10^6, na.rm = TRUE))
    
  }
  
  # Create filtered datasets for each location
  rural <- filter_data(data, ind, start, end, "RURAL")
  urban <- filter_data(data, ind, start, end, "URBAN")
  
  # Joined filtered data with world map
  plot <- rbind(rural, urban)
  
  # Create data frame with facet property
  plot <- data.frame(plot, Facet = rep(c("rural","urban"),
                                       times=c(nrow(rural),nrow(urban))))
  
  # Specify limits for plot
  x_min <- start
  #x_max <- end   note: x_max = end is default value of the function (can be overridden)
  y_min <- 3400
  y_max <- 6250
  
  # Plot graph
  plot %>%
    ggplot(aes(year, total_people_million)) +
    geom_point(size = 2) +
    geom_line(size = 0.1) +
    
    # Set limits
    scale_x_continuous("Year", breaks = seq(2000, 2015, by = 5), limits = c(x_min, x_max)) +
    scale_y_continuous("Million people", breaks = seq(3500, 6500, by = 500), limits = c(3400, 6750)) +
    
    # Labels
    labs(x = "Year") +
    
    # Theme adjustments
    theme(panel.background = element_rect(linetype = 5),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(0,34,10,6),"mm"),  # top, right, bottom, left
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12)) +
    
    # Faceting
    facet_grid(~Facet)
  
}

### Create arranged GIF of heatmap and scatterplot
gif_grid_electricity <- function(data, ind, start, end, d = 0.5, filename = "animation", w = 1440, h = 720) {
  
  # Accumulator variable for the GIF frame
  frame <- start
  
  for (i in start:end) {
    
    # Set PNG file specifications
    png(paste('./figs/', filename, '_', frame, '.png', sep = ''), width = w, height = h)
    
    # Create plots
    heatmap <- facet_heatmap(data, ind, frame)
    scatterplot <- facet_scatterplot(data, ind, start, frame, end)
    
    # Print arranged plots
    grid.arrange(heatmap, scatterplot, nrow = 2, heights = c(7/10, 3/10))
    
    # Close PNG file
    dev.off()
    
    # Move on to next frame in GIF
    frame <- frame + 1
  }
  
  # Set GIF filename
  gif_filename <- paste('./figs/', filename, '.gif', sep = '')
  
  # Create list of PNG filenames
  filenames <- as.character(seq(start, end))
  filenames <- paste('./figs/', filename, '_', filenames, '.png', sep= '')
  
  # Create GIF from PNG images
  gifski(filenames, delay = d, width = w, height = h,
         gif_file = gif_filename)
  
  # Delete PNG images
  file.remove(filenames)
  
  # Open GIF with default local application
  utils::browseURL(gif_filename)
  
}