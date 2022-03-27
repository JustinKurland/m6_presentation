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
                                start_date   = "1-09",
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
              line = list(color        = ~`1-09`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`1-09`),~min(`1-09`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`1-09`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`1-10`),~min(`1-10`)),
                #      label = '1-10', values = ~`1-10`),
                
                # # Day 3
                # list(range = c(~max(`1-11`),~min(`1-11`)),
                #      label = '1-11', values = ~`1-11`),
                
                # # Day 4
                # list(range = c(~max(`1-12`),~min(`1-12`)),
                #      label = '1-12', values = ~`1-12`),
                
                # # Day 5
                # list(range = c(~max(`1-13`),~min(`1-13`)),
                #      label = '1-13', values = ~`1-13`),
                
                # # Day 6
                # list(range = c(~max(`1-14`),~min(`1-14`)),
                #      label = '1-14', values = ~`1-14`),
                
                # # Day 7
                #list(range = c(~max(`1-16`),~min(`1-16`)),
                #     label = '1-16', values = ~`1-16`)
                
                # # Day 8
                # list(range = c(~max(`1-17`),~min(`1-17`)),
                #      label = '1-17', values = ~`1-17`),
                
                # # Day 9
                # list(range = c(~max(`1-18`),~min(`1-18`)),
                #      label = '1-18', values = ~`1-18`),
                
                # # Day 10
                # list(range = c(~max(`1-19`),~min(`1-19`)),
                #      label = '1-19', values = ~`1-19`),
                
                # # Day 11
                #list(range = c(~max(`1-20`),~min(`1-20`)),
                #     label = '1-20', values = ~`1-20`)
                
                # # Day 12
                #list(range = c(~max(`1-21`),~min(`1-21`)),
                #     label = '1-21', values = ~`1-21`)
                
                # # Day 13
                # list(range = c(~max(`1-23`),~min(`1-23`)),
                #      label = '1-23', values = ~`1-23`),
                
                # # Day 14
                # list(range = c(~max(`1-24`),~min(`1-24`)),
                #      label = '1-24', values = ~`1-24`),
                
                # # Day 15
                # list(range = c(~max(`1-25`),~min(`1-25`)),
                #      label = '1-25', values = ~`1-25`),
                
                # # Day 16
                #list(range = c(~max(`1-26`),~min(`1-26`)),
                #     label   = '1-26', values = ~`1-26`),
                
                # # Day 17
                #list(range = c(~max(`1-27`),~min(`1-27`)),
                #     label   = '1-27', values = ~`1-27`),
                
                # # Day 18
                #list(range = c(~max(`1-28`),~min(`1-28`)),
                #     label = '1-28', values = ~`1-28`)
                
                # # Day 19
                # list(range = c(~max(`1-30`),~min(`1-30`)),
                #      label = '1-30', values = ~`1-30`),
                
                # # Day 20
                # list(range = c(~max(`1-31`),~min(`1-31`)),
                #      label = '1-31', values = ~`1-31`),
                
                # # Day 21
                # list(range = c(~max(`2-01`),~min(`2-01`)),
                #      label = '2-01', values = ~`2-01`),
                
                # # Day 22
                #list(range = c(~max(`2-02`),~min(`2-02`)),
                #     label = '2-02', values = ~`2-02`),
                
                # # Day 23
                # list(range = c(~max(`2-03`),~min(`2-03`)),
                #      label = '2-03', values = ~`2-03`),
                
                # # Day 24
                # list(range = c(~max(`2-04`),~min(`2-04`)),
                #      label = '2-04', values = ~`2-04`),
                
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