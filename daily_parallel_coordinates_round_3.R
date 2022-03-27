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
                                start_date   = "5-02",
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
              line = list(color        = ~`5-02`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`5-02`),~min(`5-02`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`5-02`,
                #     name    = "Team",
                #     visible = TRUE),
                
                #  # Day 2
                #list(range = c(~max(`5-03`),~min(`5-03`)),
                #     label = '5-03', values = ~`5-03`),
                
                # # Day 3
                #list(range = c(~max(`5-04`),~min(`5-04`)),
                #     label = '5-04', values = ~`5-04`)
                
                # # Day 4
                # list(range = c(~max(`5-05`),~min(`5-05`)),
                #      label = '5-05', values = ~`5-05`),
                
                # # Day 5
                # list(range = c(~max(`5-06`),~min(`5-06`)),
                #      label = '5-06', values = ~`5-06`)
                
                # # Day 6
                # list(range = c(~max(`5-07`),~min(`5-07`)),
                #      label = '5-07', values = ~`5-07`),
                
                # # Day 7
                #list(range = c(~max(`5-09`),~min(`5-09`)),
                #     label   = '5-09', values = ~`5-09`),
                
                #  # Day 8
                #list(range = c(~max(`5-10`),~min(`5-10`)),
                #     label = '5-10', values = ~`5-10`),
                
                # # Day 9
                #list(range = c(~max(`5-11`),~min(`5-11`)),
                #     label = '5-11', values = ~`5-11`)
                
                # # Day 10
                # list(range = c(~max(`5-12`),~min(`5-12`)),
                #      label = '5-12', values = ~`5-12`),
                
                # # Day 11
                # list(range = c(~max(`5-13`),~min(`5-13`)),
                #      label = '5-13', values = ~`5-13`)
                
                # # Day 12
                # list(range = c(~max(`5-14`),~min(`5-14`)),
                #      label = '5-14', values = ~`5-14`),
                
                #  # Day 13
                #list(range = c(~max(`5-16`),~min(`5-16`)),
                #     label = '5-16', values = ~`5-16`),
                
                # # Day 14
                #list(range = c(~max(`5-17`),~min(`5-17`)),
                #     label = '5-17', values = ~`5-17`)
                
                # # Day 15
                # list(range = c(~max(`5-18`),~min(`5-18`)),
                #      label = '5-18', values = ~`5-18`),
                
                # # Day 16
                # list(range = c(~max(`5-19`),~min(`5-19`)),
                #      label = '5-19', values = ~`5-19`)
                
                # # Day 17
                # list(range = c(~max(`5-20`),~min(`5-20`)),
                #      label = '5-20', values = ~`5-20`),
                
                # # Day 18
                # list(range = c(~max(`5-21`),~min(`5-21`)),
                #      label = '5-21', values = ~`5-21`),
                
                #  # Day 19
                #list(range = c(~max(`5-23`),~min(`5-23`)),
                #     label = '5-23', values = ~`5-23`),
                
                # # Day 20
                #list(range = c(~max(`5-23`),~min(`5-23`)),
                #     label = '5-23', values = ~`5-23`)
                
                # # Day 21
                # list(range = c(~max(`5-24`),~min(`5-24`)),
                #      label = '5-24', values = ~`5-24`),
                
                # # Day 22
                # list(range = c(~max(`5-25`),~min(`5-25`)),
                #      label = '5-25', values = ~`5-25`)
                
                # # Day 23
                # list(range = c(~max(`5-26`),~min(`5-26`)),
                #      label = '5-26', values = ~`5-26`),
                
                # # Day 24
                # list(range = c(~max(`5-27`),~min(`5-27`)),
                #      label = '5-27', values = ~`5-27`),
                
                # # Day 25
                # list(range = c(~max(`5-28`),~min(`5-28`)),
                #      label = '5-28', values = ~`5-28`),
                
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