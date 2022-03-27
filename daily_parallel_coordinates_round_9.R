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
                                start_date   = "10-17",
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
              line = list(color        = ~`10-17`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`10-17`),~min(`10-17`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`10-17`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`10-18`),~min(`10-18`)),
                #      label = '10-18', values = ~`10-18`),
                
                # # Day 3
                # list(range = c(~max(`10-19`),~min(`10-19`)),
                #      label = '10-19', values = ~`10-19`),
                
                # # Day 4
                # list(range = c(~max(`10-20`),~min(`10-20`)),
                #      label = '10-20', values = ~`10-20`),
                
                # # Day 5
                # list(range = c(~max(`10-21`),~min(`10-21`)),
                #      label = '10-21', values = ~`10-21`),
                
                # # Day 6
                # list(range = c(~max(`10-22`),~min(`10-22`)),
                #      label = '10-22', values = ~`10-22`),
                
                # # Day 7
                #list(range = c(~max(`10-24`),~min(`10-24`)),
                #     label = '10-24', values = ~`10-24`)
                
                # # Day 8
                # list(range = c(~max(`10-25`),~min(`10-25`)),
                #      label = '10-25', values = ~`10-25`),
                
                # # Day 9
                # list(range = c(~max(`10-26`),~min(`10-26`)),
                #      label = '10-26', values = ~`10-26`),
                
                # # Day 10
                # list(range = c(~max(`10-27`),~min(`10-27`)),
                #      label = '10-27', values = ~`10-27`),
                
                # # Day 11
                #list(range = c(~max(`10-28`),~min(`10-28`)),
                #     label = '10-28', values = ~`10-28`)
                
                # # Day 12
                #list(range = c(~max(`10-29`),~min(`10-29`)),
                #     label = '10-29', values = ~`10-29`)
                
                # # Day 13
                # list(range = c(~max(`10-31`),~min(`10-31`)),
                #      label = '10-31', values = ~`10-31`),
                
                # # Day 14
                # list(range = c(~max(`11-01`),~min(`11-01`)),
                #      label = '11-01', values = ~`11-01`),
                
                # # Day 15
                # list(range = c(~max(`11-02`),~min(`11-02`)),
                #      label = '11-02', values = ~`11-02`),
                
                # # Day 16
                #list(range = c(~max(`11-03`),~min(`11-03`)),
                #     label   = '11-03', values = ~`11-03`),
                
                # # Day 17
                #list(range = c(~max(`11-04`),~min(`11-04`)),
                #     label   = '11-04', values = ~`11-04`),
                
                # # Day 18
                #list(range = c(~max(`11-05`),~min(`11-05`)),
                #     label = '11-05', values = ~`11-05`)
                
                # # Day 19
                # list(range = c(~max(`11-07`),~min(`11-07`)),
                #      label = '11-07', values = ~`11-07`),
                
                # # Day 20
                # list(range = c(~max(`11-08`),~min(`11-08`)),
                #      label = '11-08', values = ~`11-08`),
                
                # # Day 21
                # list(range = c(~max(`11-09`),~min(`11-09`)),
                #      label = '11-09', values = ~`11-09`),
                
                # # Day 22
                #list(range = c(~max(`11-10`),~min(`11-10`)),
                #     label = '11-10', values = ~`11-10`),
                
                # # Day 23
                # list(range = c(~max(`11-11`),~min(`11-11`)),
                #      label = '11-11', values = ~`11-11`),
                
                # # Day 24
                # list(range = c(~max(`11-12`),~min(`11-12`)),
                #      label = '11-12', values = ~`11-12`),
                
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