# Libraries ----
suppressPackageStartupMessages({
  if(!require(tidyverse)) install.packages('tidyverse')
  library(tidyverse)
  if(!require(lubridate)) install.packages('lubridate')
  library(lubridate)
  if(!require(plotly)) install.packages('plotly')
  library(plotly)
})


# Read and Wrangle ----
df <- read.csv("M6_Leaderboard.csv") %>%
  as_tibble() %>%
  mutate(Date = as.Date(mdy(Date))) %>%
  setNames(c("Date","Position" ,"Team","Overall Rank (OR)", "Performance of Forecasts (RPS)", "Rank (Forecasts)", "Performance of Decisions (IR)", "Rank (Decisions)")) %>%
  mutate(Month = month(Date)) %>%
  mutate(Day   = day(Date)) %>%
  mutate(MD    = str_c(Month, "-", Day)) %>%
  select(MD, Position, "Overall Rank (OR)", Team, "Rank (Forecasts)", "Rank (Decisions)")

# Parallel Coord Function ---- 
plot_parallel_coord <- function(data,
                                kpi          = "Position",
                                start_date   = "4-04",
                                colorscale   = 'YlOrRd',
                                showscale    = TRUE,
                                reversescale = FALSE,
                                top_rank     = 0,
                                bottom_rank  = 175,
                                plot_width   = 500,
                                plot_height  = 600
) {
  
  # Import Data 
  g <- data %>%
    
    # Select Columns of Interest
    select(MD, kpi, Team) %>%
    
    # Pivot Wider
    pivot_wider(names_from = MD, values_from = kpi) %>%
    
    # Input Width and Height
    plot_ly(width = plot_width, height = plot_height) %>%
    
    # Add Trace
    add_trace(type = 'parcoords',
              line = list(color        = ~`4-04`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`4-04`),~min(`4-04`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`4-04`,
                #     name    = "Team",
                #     visible = TRUE),
                
                #  # Day 2
                #list(range = c(~max(`4-05`),~min(`4-05`)),
                #     label = '4-05', values = ~`3-18`),
                
                # # Day 3
                #list(range = c(~max(`4-06`),~min(`4-06`)),
                #     label = '4-06', values = ~`4-06`)
                
                # # Day 4
                # list(range = c(~max(`4-07`),~min(`4-07`)),
                #      label = '4-07', values = ~`4-07`),
                
                # # Day 5
                # list(range = c(~max(`4-08`),~min(`4-08`)),
                #      label = '4-08', values = ~`4-08`)
                
                # # Day 6
                # list(range = c(~max(`4-09`),~min(`4-09`)),
                #      label = '4-09', values = ~`4-09`),
                
                # # Day 7
                #list(range = c(~max(`4-11`),~min(`4-11`)),
                #     label   = '4-11', values = ~`4-11`),

                #  # Day 8
                #list(range = c(~max(`4-12`),~min(`4-12`)),
                #     label = '4-12', values = ~`4-12`),
                
                # # Day 9
                #list(range = c(~max(`4-13`),~min(`4-13`)),
                #     label = '4-13', values = ~`4-13`)
                
                # # Day 10
                # list(range = c(~max(`4-14`),~min(`4-14`)),
                #      label = '4-14', values = ~`4-14`),
                
                # # Day 11
                # list(range = c(~max(`4-15`),~min(`4-15`)),
                #      label = '4-15', values = ~`4-15`)
                
                # # Day 12
                # list(range = c(~max(`4-16`),~min(`4-16`)),
                #      label = '4-16', values = ~`4-16`),
                
                #  # Day 13
                #list(range = c(~max(`4-18`),~min(`4-18`)),
                #     label = '4-18', values = ~`4-18`),
                
                # # Day 14
                #list(range = c(~max(`4-19`),~min(`4-19`)),
                #     label = '4-19', values = ~`4-19`)
                
                # # Day 15
                # list(range = c(~max(`4-20`),~min(`4-20`)),
                #      label = '4-20', values = ~`4-20`),
                
                # # Day 16
                # list(range = c(~max(`4-21`),~min(`4-21`)),
                #      label = '4-21', values = ~`4-21`)
                
                # # Day 17
                # list(range = c(~max(`4-22`),~min(`4-22`)),
                #      label = '4-22', values = ~`4-22`),
                
                # # Day 18
                # list(range = c(~max(`4-23`),~min(`4-23`)),
                #      label = '4-23', values = ~`4-23`),
                
                #  # Day 19
                #list(range = c(~max(`4-25`),~min(`4-25`)),
                #     label = '4-25', values = ~`4-25`),
                
                # # Day 20
                #list(range = c(~max(`4-26`),~min(`4-26`)),
                #     label = '4-26', values = ~`4-26`)
                
                # # Day 21
                # list(range = c(~max(`4-27`),~min(`4-27`)),
                #      label = '4-27', values = ~`4-27`),
                
                # # Day 22
                # list(range = c(~max(`4-28`),~min(`4-28`)),
                #      label = '4-28', values = ~`4-28`)
                
                # # Day 23
                # list(range = c(~max(`4-29`),~min(`4-29`)),
                #      label = '4-22', values = ~`4-22`),
                
                # # Day 24
                # list(range = c(~max(`4-30`),~min(`4-30`)),
                #      label = '4-30', values = ~`4-30`)
                
              )
    ) %>%
    
    layout(title = kpi)
  
  return(g)
  
  
}


# Position
plot_parallel_coord(df, kpi = "Position")

# Overall Rank
plot_parallel_coord(df, kpi = "Overall Rank (OR)")

# Rank (Forecasts)
plot_parallel_coord(df, kpi = "Rank (Forecasts)")

# Rank (Decisions)
plot_parallel_coord(df, kpi = "Rank (Decisions)")

