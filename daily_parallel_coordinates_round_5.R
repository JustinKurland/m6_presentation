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
                                start_date   = "6-27",
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
              line = list(color        = ~`6-27`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`6-27`),~min(`6-27`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`5-30`,
                #     name    = "Team",
                #     visible = TRUE),
                
                #  # Day 2
                #list(range = c(~max(`6-28`),~min(`6-28`)),
                #     label = '5-31', values = ~`5-31`),
                
                # # Day 3
                #list(range = c(~max(`6-29`),~min(`6-29`)),
                #     label = '6-29', values = ~`6-29`)
                
                # # Day 4
                # list(range = c(~max(`6-30`),~min(`6-30`)),
                #      label = '6-30', values = ~`6-30`),
                
                # # Day 5
                #list(range = c(~max(`7-01`),~min(`7-01`)),
                #     label = '7-01', values = ~`7-01`)
                
                # # Day 6
                # list(range = c(~max(`7-02`),~min(`7-02`)),
                #      label = '7-02', values = ~`7-02`),
                
                # # Day 7
                # list(range = c(~max(`7-04`),~min(`7-04`)),
                #      label = '7-04', values = ~`7-04`),
                
                # # Day 8
                # list(range = c(~max(`7-05`),~min(`7-05`)),
                #      label = '7-04', values = ~`7-04`),
                
                # # Day 9
                #list(range = c(~max(`7-06`),~min(`7-06`)),
                #     label   = '7-06', values = ~`7-06`),
                
                # # Day 10
                #list(range = c(~max(`7-07`),~min(`7-07`)),
                #     label = '7-07', values = ~`7-07`),
                
                # # Day 11
                #list(range = c(~max(`7-08`),~min(`7-08`)),
                #     label = '7-08', values = ~`7-08`)
                
                # # Day 12
                # list(range = c(~max(`7-09`),~min(`7-09`)),
                #      label = '7-09', values = ~`7-09`),
                
                # # Day 13
                # list(range = c(~max(`7-11`),~min(`7-11`)),
                #      label = '7-11', values = ~`7-11`),
                
                # # Day 14
                # list(range = c(~max(`7-12`),~min(`7-12`)),
                #      label = '7-12', values = ~`7-12`),
                
                #  # Day 15
                #list(range = c(~max(`7-13`),~min(`7-13`)),
                #     label = '7-13', values = ~`7-13`),
                
                # # Day 16
                #list(range = c(~max(`7-14`),~min(`7-14`)),
                #     label = '7-14', values = ~`7-14`)
                
                # # Day 17
                # list(range = c(~max(`7-15`),~min(`7-15`)),
                #      label = '7-15', values = ~`7-15`),
                
                # # Day 18
                # list(range = c(~max(`7-16`),~min(`7-16`)),
                #      label = '7-16', values = ~`7-16`)
                
                # # Day 19
                # list(range = c(~max(`7-18`),~min(`7-18`)),
                #      label = '7-18', values = ~`7-18`),
                
                # # Day 20
                # list(range = c(~max(`7-19`),~min(`7-19`)),
                #      label = '7-19', values = ~`7-19`),
                
                # # Day 21
                #list(range = c(~max(`7-20`),~min(`7-20`)),
                #     label = '7-20', values = ~`7-20`),
                
                # # Day 22
                #list(range = c(~max(`7-21`),~min(`7-21`)),
                #     label = '7-21', values = ~`7-21`)
                
                # # Day 23
                # list(range = c(~max(`7-22`),~min(`7-22`)),
                #      label = '7-22', values = ~`7-22`),
                
                # # Day 24
                # list(range = c(~max(`7-23`),~min(`7-23`)),
                #      label = '7-23', values = ~`7-23`)
                
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