######################################
# Functions
######################################


############   FILTERING   ############


### Filter for given indicator; delete empty columns
select_ind <- function(data, ind) {
  
  data %>%
    filter(indicator == ind) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


### Filter for given indicator and year; delete empty columns
select_ind_year <- function(data, ind, yr) {
  
  data %>%
    filter(indicator == ind, year %in% yr) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


### Filter for given indicator, year and loc; delete empty columns
select_ind_year_loc <- function(data, ind, yr, loc) {
  
  data %>%
    filter(indicator == ind, year %in% yr, location == loc) %>%
    select_if(~sum(!is.na(.)) > 0)
  
}


############   PLOTTING   ############


### Create world heatmap
plot_world_heatmap <- function(data, ind, year) {
  
  # Create filtered dataset
  data_filtered <- select_ind_year(data, ind, year)
  
  # Join filtered dataset with world map
  map_data <- left_join(map_world, data_filtered, by = 'country')
  
  # Text for title and legend
  title <- map_data$seriesdescription %>% na.omit() %>% unique()
  unit <- map_data$units %>% na.omit() %>% unique() %>% as.character()
  
  # Plot graph
  ggplot(map_data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = value)) +
    
    # Titles and axis
    labs(fill = paste(unit),
         title = paste(title),
         subtitle = paste('Year', year),
         x = NULL, y = NULL) +
    
    # Legend
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = 'lightgrey') +
    
    # Theme adjustments
    theme(plot.title = element_text(size = 9, face = 'bold'),
          plot.subtitle = element_text(size = 7, hjust = 0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.key.size = unit(6, 'pt'),
          legend.title = element_text(size = 3),
          legend.text = element_text(size = 4),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA))
  
}


save_last_plot <- function(filename, w = 1280, h = 720, dpi = 300) {
  
  # Convert width and heigh from pixels into cm because ggsave() doesn't accept pixels as a unit
  w_mm <- w/dpi
  h_mm <- h/dpi
  
  ggsave(filename = paste(filename, '.png', sep = ''),
         plot = last_plot(),
         path = './figs/',
         device = 'png',
         dpi = dpi,
         width = w_mm, height = h_mm, units = 'in')
  
}


############ CREATE GIF ############


### Create GIF of plot_world_heatmap series
gif_world_heatmap <- function(data, ind, start, end, filename = "animation", w = 1280, h = 720, d = 0.5) {
  
  # Create ggplots for interval given in arguments
  for (yr in start:end) {
    # Set PNG file specifications
    png(paste('./figs/', as.character(filename), '_', yr, '.png', sep= ''), width = w, height = h)
    # Print map to file
    print(plot_world_heatmap(data, ind, yr))
    # Close file
    dev.off()
  }
  
  # Create list of PNG filenames
  filenames <- as.character(seq(start, end))
  filenames <- paste('./figs/', filename, '_', filenames, '.png', sep= '')
  
  # Create GIF filename
  gif_filename <- paste('./figs/', filename, '.gif', sep = '')
  
  # Create GIF from PNG images
  gifski(filenames, delay = d, width = w, height = h,
         gif_file = gif_filename)
  
  # Delete PNG images
  file.remove(filenames)
  
  # Open GIF with default local application
  utils::browseURL(gif_filename)
  
}

