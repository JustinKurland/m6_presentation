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
                                start_date   = "11-14",
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
              line = list(color        = ~`11-14`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`11-14`),~min(`11-14`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`11-14`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`11-15`),~min(`11-15`)),
                #      label = '11-15', values = ~`11-15`),
                
                # # Day 3
                # list(range = c(~max(`11-16`),~min(`11-16`)),
                #      label = '11-16', values = ~`11-16`),
                
                # # Day 4
                # list(range = c(~max(`11-17`),~min(`11-17`)),
                #      label = '11-17', values = ~`11-17`),
                
                # # Day 5
                # list(range = c(~max(`11-18`),~min(`11-18`)),
                #      label = '11-18', values = ~`11-18`),
                
                # # Day 6
                # list(range = c(~max(`11-19`),~min(`11-19`)),
                #      label = '11-19', values = ~`11-19`),
                
                # # Day 7
                #list(range = c(~max(`11-21`),~min(`11-21`)),
                #     label = '11-21', values = ~`11-21`)
                
                # # Day 8
                # list(range = c(~max(`11-22`),~min(`11-22`)),
                #      label = '11-22', values = ~`11-22`),
                
                # # Day 9
                # list(range = c(~max(`11-23`),~min(`11-23`)),
                #      label = '11-23', values = ~`11-23`),
                
                # # Day 10
                # list(range = c(~max(`11-24`),~min(`11-24`)),
                #      label = '11-24', values = ~`11-24`),
                
                # # Day 11
                #list(range = c(~max(`11-25`),~min(`11-25`)),
                #     label = '11-25', values = ~`11-25`)
                
                # # Day 12
                #list(range = c(~max(`11-26`),~min(`11-26`)),
                #     label = '11-26', values = ~`11-26`)
                
                # # Day 13
                # list(range = c(~max(`11-28`),~min(`11-28`)),
                #      label = '11-28', values = ~`11-28`),
                
                # # Day 14
                # list(range = c(~max(`11-29`),~min(`11-29`)),
                #      label = '11-29', values = ~`11-29`),
                
                # # Day 15
                # list(range = c(~max(`11-30`),~min(`11-30`)),
                #      label = '11-30', values = ~`11-30`),
                
                # # Day 16
                #list(range = c(~max(`12-01`),~min(`12-01`)),
                #     label   = '12-01', values = ~`12-01`),
                
                # # Day 17
                #list(range = c(~max(`12-02`),~min(`12-02`)),
                #     label   = '12-02', values = ~`12-02`),
                
                # # Day 18
                #list(range = c(~max(`12-03`),~min(`12-03`)),
                #     label = '12-03', values = ~`12-03`)
                
                # # Day 19
                # list(range = c(~max(`12-05`),~min(`12-05`)),
                #      label = '12-05', values = ~`12-05`),
                
                # # Day 20
                # list(range = c(~max(`12-06`),~min(`12-06`)),
                #      label = '12-06', values = ~`12-06`),
                
                # # Day 21
                # list(range = c(~max(`12-07`),~min(`12-07`)),
                #      label = '12-07', values = ~`12-07`),
                
                # # Day 22
                #list(range = c(~max(`12-08`),~min(`12-08`)),
                #     label = '12-08', values = ~`12-08`),
                
                # # Day 23
                # list(range = c(~max(`12-09`),~min(`12-09`)),
                #      label = '12-09', values = ~`12-09`),
                
                # # Day 24
                # list(range = c(~max(`12-10`),~min(`12-10`)),
                #      label = '12-10', values = ~`12-10`),
                
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