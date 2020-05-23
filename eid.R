# inspired from: https://rstudio-pubs-static.s3.amazonaws.com/237347_4375e98b087f444fbac540a9e6fc674b.html

library(plotly)
#devtools::install_github("ropensci/plotly")
rm(list = ls())
gc()

# Options for plotting ----
x <- 0.2
y <- 0.72
speed <- 250
nbkdrops <- 100

# Colorset for plot
# See http://colorhunt.co/
cols <- c("#ffffb3", "#33a02c", "#a6cee3", "#f4cae4") # "#fb9a99"
ncolors <- length(cols)


# Function to create random points by adding jitter to ----
# a starting set of points
n <- 1000  # Number of points

# Starting template
bkdrop.x <- runif(n, min = 0, max = 1)
bkdrop.y <- runif(n, min = 0, max = 1)

# Function Definition
bkdrop <- function(n = 100, amount = 0.005){
  
  x <- jitter(bkdrop.x, amount = amount)
  y <- jitter(bkdrop.y, amount = amount)
  
  df <- data.frame(x, y)
  
  return(df)
  
}

# Make backdrops ----
# Each call to the backdrop function is a separate frame
# Number of frames is controlled by nbkdrops

bkdrop.df <- data.frame()
for(i in 1:nbkdrops){
  temp <- bkdrop()
  temp <- data.frame(temp, frame = i, color = sample(1:ncolors, size = nrow(temp), replace = T))
  bkdrop.df <- rbind(bkdrop.df, temp)
  
}

# Make back lights ----
# Coordinates for backlight rectangles
# Will be plotted as line segments
# 1 = yellow, 2 = green, 3 = blue
bklight.x <- c(0.28, 0.18, 0.36)
bklight.y <- c(0.42, 0.62, 0.65)
bklight.xend <- c(0.70, 0.36, 0.80)
bklight.yend <- bklight.y

# Function to create a dataframe containing coordinates, frame and
# color of each backlight segment
makebklight <- function(id){
  bklight <- data.frame()
  
  for(i in 1:nbkdrops){
    temp <- data.frame(x = bklight.x[id],
                       y = bklight.y[id],
                       xend = bklight.xend[id],
                       yend = bklight.yend[id],
                       frame = i, 
                       color = sample(1:ncolors, size = 1))
    
    bklight <- rbind(bklight, temp)
  }
  
  return(bklight)
}

# Create backlight segments
bklight1 <- makebklight(1)
bklight2 <- makebklight(2)
bklight3 <- makebklight(3)
bklight4 <- makebklight(4)

# Initialize colors for first frame
bklight1$color[1] <- 1
bklight2$color[1] <- 2
bklight3$color[1] <- 3
bklight4$color[1] <- 4

# Plot !! ----
p <- plot_ly(height = 800, width = 1024, 
             colors = cols, 
             frame = ~frame,
             x = ~x, 
             y = ~y,
             color = ~factor(color)) %>%  
  
  # Backdrop
  add_markers(data = bkdrop.df, 
              opacity = 0.8,
              marker = list(symbol = "star", size = 8),
              hoverinfo = "none") %>%
  
  # Add segments (for back lighting)
  add_segments(data = bklight1, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>%
  
  add_segments(data = bklight2, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>% 
  
  add_segments(data = bklight3, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 160)) %>% 

  
  # Animation options
  # See https://cpsievert.github.io/plotly_book/key-frame-animations.html
  
  animation_opts(speed, easing = "linear", transition = 0) %>%
  animation_button(x = 1, xanchor = "right", y = 1, yanchor = "bottom") %>%
  animation_slider(hide = T) %>%
  
  # Layout, annotations and shapes
  
  layout(
    showlegend = F,
    
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    
    images = list(
      list(source = "https://raw.githubusercontent.com/erikaris/scripting_for_fun/master/footer.png",
           xref = "paper",
           yref = "paper",
           x= 0,
           y= 0.1,
           sizex = 1,
           sizey = 0.5,
           opacity = 1
      ), 
      
      list(source = "https://raw.githubusercontent.com/erikaris/scripting_for_fun/master/ketupat2.png",
           xref = "paper",
           yref = "paper",
           x= 0.8,
           y= 0.9,
           sizex = 0.3,
           sizey = 0.3,
           opacity = 1
      ), 
      
      list(source = "https://raw.githubusercontent.com/erikaris/scripting_for_fun/master/sticker_rladies_jkt2.png",
           xref = "paper",
           yref = "paper",
           x= 0.05,
           y= 0.95,
           sizex = 0.2,
           sizey = 0.2,
           opacity = 1
      )
      ),
    
    annotations = list(
      
      # For shadow
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.002, y = y + 0.002, 
           showarrow = F,
           text = "Eid Mubarak<br>1441H!",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.003, y = y + 0.003, 
           showarrow = F,
           text = "Eid Mubarak<br>1441H!",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.004, y = y + 0.004, 
           showarrow = F,
           text = "Eid Mubarak<br>1441H!",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.004, y = y + 0.004, 
           showarrow = F,
           text = "Eid Mubarak<br>1441H!",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      # Actual
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x, y = y, 
           showarrow = F,
           text = "Eid Mubarak<br>1441H!",
           font = list(size = 100, family = "Times New Roman",
                       color = "#1f78b4"))
    ),
    
    shapes = list(
      
      # Border
      list(xref = "paper", yref = "paper",
           x0 = 0, y0 = 0, 
           x1 = 1, y1 = 1,
           type = "rect",
           line = list(width = 10, color = cols[1])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.01, y0 = 0.01, 
           x1 = 0.99, y1 = 0.99,
           type = "rect",
           line = list(width = 10, color = cols[2])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.02, y0 = 0.02, 
           x1 = 0.98, y1 = 0.98,
           type = "rect",
           line = list(width = 10, color = cols[3])),
      
      # list(xref = "paper", yref = "paper",
      #      x0 = 0.03, y0 = 0.03, 
      #      x1 = 0.97, y1 = 0.97,
      #      type = "rect",
      #      line = list(width = 10, color = cols[4])),
      
      # Black outline
      # list(xref = "plot", yref = "plot",
      #      path = "
      #      M 0.50 0.53
      #      L 0.50 0.50
      #      L 0.18 0.50 
      #      L 0.18 0.73
      #      L 0.48, 0.73",
      #      type = "path",
      #      line = list(width = 7, color = "black")),
      
      # list(xref = "plot", yref = "plot",
      #      path = "
      #      M 0.50 0.535
      #      L 0.48 0.535
      #      L 0.48 0.77
      #      L 0.80 0.77
      #      L 0.80 0.535
      #      Z",
      #      type = "path",
      #      line = list(width = 7, color = "black")),
      
      list(xref = "plot", yref = "plot",
           path = "
           M 0.28 0.5
           L 0.28 0.31
           L 0.63 0.31
           L 0.63 0.535",
           type = "path",
           line = list(width = 7, color = "black"))
      
      # list(xref = "plot", yref = "plot",
      #      path = "
      #      M 0.28 0.5
      #      L 0.58 0.41
      #      L 0.83 0.91
      #      L 0.23 0.235",
      #      type = "path",
      #      line = list(width = 7, color = "black"))
      
    )
  )

p