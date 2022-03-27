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
                                start_date   = "7-25",
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
              line = list(color        = ~`7-25`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                
                # # Day 1
                #list(range = c(~max(`7-25`),~min(`7-25`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`7-25`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`7-26`),~min(`7-26`)),
                #      label = '7-26', values = ~`7-26`),
                
                # # Day 3
                # list(range = c(~max(`7-27`),~min(`7-27`)),
                #      label = '7-27', values = ~`7-27`),
                
                # # Day 4
                # list(range = c(~max(`7-28`),~min(`7-28`)),
                #      label = '7-28', values = ~`7-28`),
                
                # # Day 5
                #list(range = c(~max(`7-29`),~min(`7-29`)),
                #     label = '7-29', values = ~`7-29`)
                
                # # Day 6
                # list(range = c(~max(`7-30`),~min(`7-30`)),
                #      label = '7-30', values = ~`7-30`),
                
                # # Day 7
                #list(range = c(~max(`8-01`),~min(`8-01`)),
                #     label = '8-01', values = ~`8-01`)
                
                # # Day 8
                # list(range = c(~max(`8-02`),~min(`8-02`)),
                #      label = '8-02', values = ~`8-02`),
                
                # # Day 9
                # list(range = c(~max(`8-03`),~min(`8-03`)),
                #      label = '8-03', values = ~`8-03`),
                
                # # Day 10
                # list(range = c(~max(`8-04`),~min(`8-04`)),
                #      label = '8-04', values = ~`8-04`),
                
                # # Day 11
                # list(range = c(~max(`8-05`),~min(`8-05`)),
                #      label = '8-04', values = ~`8-04`),
                
                # # Day 12
                #list(range = c(~max(`8-06`),~min(`8-06`)),
                #     label   = '8-06', values = ~`8-06`),
                
                # # Day 13
                #list(range = c(~max(`8-08`),~min(`8-08`)),
                #     label = '8-08', values = ~`8-08`)
                
                # # Day 14
                # list(range = c(~max(`8-09`),~min(`8-09`)),
                #      label = '8-09', values = ~`8-09`),
                
                # # Day 15
                # list(range = c(~max(`8-10`),~min(`8-10`)),
                #      label = '8-10', values = ~`8-10`),
                
                # # Day 16
                # list(range = c(~max(`8-11`),~min(`8-11`)),
                #      label = '8-11', values = ~`8-11`),
                
                # # Day 17
                # list(range = c(~max(`8-12`),~min(`8-12`)),
                #      label = '8-12', values = ~`8-12`),
                
                #  # Day 18
                #list(range = c(~max(`8-13`),~min(`8-13`)),
                #     label = '8-13', values = ~`8-13`),
                
                # # Day 19
                # list(range = c(~max(`8-15`),~min(`8-15`)),
                #      label = '8-15', values = ~`8-15`),
                
                # # Day 20
                # list(range = c(~max(`8-16`),~min(`8-16`)),
                #      label = '8-16', values = ~`8-16`)
                
                # # Day 21
                # list(range = c(~max(`8-17`),~min(`8-17`)),
                #      label = '8-17', values = ~`8-17`)
                
                # # Day 22
                # list(range = c(~max(`8-18`),~min(`8-18`)),
                #      label = '8-18', values = ~`8-18`),
                
                # # Day 23
                # list(range = c(~max(`8-19`),~min(`8-19`)),
                #      label = '8-19', values = ~`8-19`),
                
                # # Day 24
                #list(range = c(~max(`8-20`),~min(`8-20`)),
                #     label = '8-20', values = ~`8-20`),
                
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