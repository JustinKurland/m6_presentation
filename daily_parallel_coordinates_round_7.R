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
                                start_date   = "8-22",
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
              line = list(color        = ~`8-22`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`8-22`),~min(`8-22`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`8-22`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`8-23`),~min(`8-23`)),
                #      label = '8-23', values = ~`8-23`),
                
                # # Day 3
                # list(range = c(~max(`8-24`),~min(`8-24`)),
                #      label = '8-24', values = ~`8-24`),
                
                # # Day 4
                # list(range = c(~max(`8-25`),~min(`8-25`)),
                #      label = '8-25', values = ~`8-25`),
                
                # # Day 5
                #list(range = c(~max(`8-26`),~min(`8-26`)),
                #     label = '8-26', values = ~`8-26`)
                
                # # Day 6
                # list(range = c(~max(`8-27`),~min(`8-27`)),
                #      label = '8-27', values = ~`8-27`),
                
                # # Day 7
                # list(range = c(~max(`8-29`),~min(`8-29`)),
                #      label = '8-29', values = ~`8-29`),
                
                # # Day 8
                #list(range = c(~max(`8-30`),~min(`8-30`)),
                #     label = '8-30', values = ~`8-30`)
                
                # # Day 9
                #list(range = c(~max(`8-31`),~min(`8-31`)),
                #     label = '8-31', values = ~`8-31`)
                
                # # Day 10
                #list(range = c(~max(`9-01`),~min(`9-01`)),
                #     label = '9-01', values = ~`9-01`)
                
                # # Day 11
                # list(range = c(~max(`9-02`),~min(`9-02`)),
                #      label = '9-02', values = ~`9-02`),
                
                # # Day 12
                # list(range = c(~max(`9-03`),~min(`9-03`)),
                #      label = '9-03', values = ~`9-03`),
                
                # # Day 13
                # list(range = c(~max(`9-05`),~min(`9-05`)),
                #      label = '9-04', values = ~`9-04`),
                
                # # Day 14
                #list(range = c(~max(`9-06`),~min(`9-06`)),
                #     label   = '9-06', values = ~`9-06`),
              
                # # Day 15
                #list(range = c(~max(`9-07`),~min(`9-07`)),
                #     label   = '9-07', values = ~`9-07`),
                
                # # Day 16
                #list(range = c(~max(`9-08`),~min(`9-08`)),
                #     label = '9-08', values = ~`9-08`)
                
                # # Day 17
                # list(range = c(~max(`9-09`),~min(`9-09`)),
                #      label = '9-09', values = ~`9-09`),
                
                # # Day 18
                # list(range = c(~max(`9-10`),~min(`9-10`)),
                #      label = '9-10', values = ~`9-10`),
                
                # # Day 19
                # list(range = c(~max(`9-12`),~min(`9-12`)),
                #      label = '9-12', values = ~`9-12`),
                
                #  # Day 20
                #list(range = c(~max(`9-13`),~min(`9-13`)),
                #     label = '9-13', values = ~`9-13`),
                
                # # Day 21
                # list(range = c(~max(`9-14`),~min(`9-14`)),
                #      label = '9-14', values = ~`9-14`),
                
                # # Day 22
                # list(range = c(~max(`9-15`),~min(`9-15`)),
                #      label = '9-15', values = ~`9-15`),
                
                # # Day 23
                # list(range = c(~max(`9-16`),~min(`9-16`)),
                #      label = '9-16', values = ~`9-16`)
                
                # # Day 24
                # list(range = c(~max(`9-17`),~min(`9-17`)),
                #      label = '9-17', values = ~`9-17`)
              
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