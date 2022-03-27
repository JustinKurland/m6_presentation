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
                                start_date   = "5-30",
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
              line = list(color        = ~`5-30`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`5-30`),~min(`5-30`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`5-30`,
                #     name    = "Team",
                #     visible = TRUE),
                
                #  # Day 2
                #list(range = c(~max(`5-31`),~min(`5-31`)),
                #     label = '5-31', values = ~`5-31`),
                
                # # Day 3
                #list(range = c(~max(`6-01`),~min(`6-01`)),
                #     label = '6-01', values = ~`6-01`)
                
                # # Day 4
                # list(range = c(~max(`6-02`),~min(`6-02`)),
                #      label = '6-02', values = ~`6-02`),
                
                # # Day 5
                # list(range = c(~max(`6-03`),~min(`6-03`)),
                #      label = '6-03', values = ~`6-03`)
                
                # # Day 6
                # list(range = c(~max(`6-04`),~min(`6-04`)),
                #      label = '6-04', values = ~`6-04`),
                
                # # Day 7
                #list(range = c(~max(`6-06`),~min(`6-06`)),
                #     label   = '6-06', values = ~`6-06`),
                
                #  # Day 8
                #list(range = c(~max(`6-07`),~min(`6-07`)),
                #     label = '6-07', values = ~`6-07`),
                
                # # Day 9
                #list(range = c(~max(`6-08`),~min(`6-08`)),
                #     label = '6-08', values = ~`6-08`)
                
                # # Day 10
                # list(range = c(~max(`6-09`),~min(`6-09`)),
                #      label = '6-09', values = ~`6-09`),
                
                # # Day 11
                # list(range = c(~max(`6-10`),~min(`6-10`)),
                #      label = '6-10', values = ~`6-10`)
                
                # # Day 12
                # list(range = c(~max(`6-11`),~min(`6-11`)),
                #      label = '6-11', values = ~`6-11`),
                
                #  # Day 13
                #list(range = c(~max(`6-13`),~min(`6-13`)),
                #     label = '6-13', values = ~`6-13`),
                
                # # Day 14
                #list(range = c(~max(`6-14`),~min(`6-14`)),
                #     label = '6-14', values = ~`6-14`)
                
                # # Day 15
                # list(range = c(~max(`6-15`),~min(`6-15`)),
                #      label = '6-15', values = ~`6-15`),
                
                # # Day 16
                # list(range = c(~max(`6-16`),~min(`6-16`)),
                #      label = '6-16', values = ~`6-16`)
                
                # # Day 17
                # list(range = c(~max(`6-17`),~min(`6-17`)),
                #      label = '6-17', values = ~`6-17`),
                
                # # Day 18
                # list(range = c(~max(`6-18`),~min(`6-18`)),
                #      label = '6-18', values = ~`6-18`),
                
                #  # Day 19
                #list(range = c(~max(`6-20`),~min(`6-20`)),
                #     label = '6-20', values = ~`6-20`),
                
                # # Day 20
                #list(range = c(~max(`6-21`),~min(`6-21`)),
                #     label = '6-21', values = ~`6-21`)
                
                # # Day 21
                # list(range = c(~max(`6-22`),~min(`6-22`)),
                #      label = '6-22', values = ~`6-22`),
                
                # # Day 22
                # list(range = c(~max(`6-23`),~min(`6-23`)),
                #      label = '6-23', values = ~`6-23`)
                
                # # Day 23
                # list(range = c(~max(`6-24`),~min(`6-24`)),
                #      label = '6-24', values = ~`6-24`),
                
                # # Day 24
                # list(range = c(~max(`6-25`),~min(`6-25`)),
                #      label = '6-25', values = ~`6-25`),
                
                # # Day 25
                # list(range = c(~max(`6-28`),~min(`6-28`)),
                #      label = '6-28', values = ~`6-28`),
                
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