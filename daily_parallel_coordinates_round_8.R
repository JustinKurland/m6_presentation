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
                                start_date   = "9-19",
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
              line = list(color        = ~`9-19`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`9-19`),~min(`9-19`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`9-19`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`9-20`),~min(`9-20`)),
                #      label = '9-20', values = ~`9-20`),
                
                # # Day 3
                # list(range = c(~max(`9-21`),~min(`9-21`)),
                #      label = '9-21', values = ~`9-21`),
                
                # # Day 4
                # list(range = c(~max(`9-22`),~min(`9-22`)),
                #      label = '9-22', values = ~`9-22`),
                
                # # Day 5
                # list(range = c(~max(`9-23`),~min(`9-23`)),
                #      label = '9-23', values = ~`9-23`),
                
                # # Day 6
                # list(range = c(~max(`9-24`),~min(`9-24`)),
                #      label = '9-24', values = ~`9-24`),
                
                # # Day 7
                #list(range = c(~max(`9-26`),~min(`9-26`)),
                #     label = '9-26', values = ~`9-26`)
                
                # # Day 8
                # list(range = c(~max(`9-27`),~min(`9-27`)),
                #      label = '9-27', values = ~`9-27`),
                
                # # Day 9
                # list(range = c(~max(`9-28`),~min(`9-28`)),
                #      label = '9-28', values = ~`9-28`),
                
                # # Day 10
                # list(range = c(~max(`9-29`),~min(`9-29`)),
                #      label = '9-29', values = ~`9-29`),
                
                # # Day 11
                #list(range = c(~max(`9-30`),~min(`9-30`)),
                #     label = '9-30', values = ~`9-30`)
                
                # # Day 12
                #list(range = c(~max(`10-01`),~min(`10-01`)),
                #     label = '10-01', values = ~`10-01`)
                
                # # Day 13
                # list(range = c(~max(`10-03`),~min(`10-03`)),
                #      label = '10-03', values = ~`10-03`),
                
                # # Day 14
                # list(range = c(~max(`10-04`),~min(`10-04`)),
                #      label = '10-04', values = ~`10-04`),
                
                # # Day 15
                # list(range = c(~max(`10-05`),~min(`10-05`)),
                #      label = '10-04', values = ~`10-04`),
                
                # # Day 16
                #list(range = c(~max(`10-06`),~min(`10-06`)),
                #     label   = '10-06', values = ~`10-06`),
                
                # # Day 17
                #list(range = c(~max(`10-07`),~min(`10-07`)),
                #     label   = '10-07', values = ~`10-07`),
                
                # # Day 18
                #list(range = c(~max(`10-08`),~min(`10-08`)),
                #     label = '10-08', values = ~`10-08`)
                
                # # Day 19
                # list(range = c(~max(`10-10`),~min(`10-10`)),
                #      label = '10-10', values = ~`10-10`),
                
                # # Day 20
                # list(range = c(~max(`10-11`),~min(`10-11`)),
                #      label = '10-11', values = ~`10-11`),
                
                # # Day 21
                # list(range = c(~max(`10-12`),~min(`10-12`)),
                #      label = '10-12', values = ~`10-12`),
                
                # # Day 22
                #list(range = c(~max(`10-13`),~min(`10-13`)),
                #     label = '10-13', values = ~`10-13`),
                
                # # Day 23
                # list(range = c(~max(`10-14`),~min(`10-14`)),
                #      label = '10-14', values = ~`10-14`),
                
                # # Day 24
                # list(range = c(~max(`10-15`),~min(`10-15`)),
                #      label = '10-15', values = ~`10-15`),
                
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