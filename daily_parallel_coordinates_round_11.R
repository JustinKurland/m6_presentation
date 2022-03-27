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
                                start_date   = "12-12",
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
              line = list(color        = ~`12-12`,
                          colorscale   = colorscale,
                          showscale    = showscale,
                          reversescale = reversescale,
                          cmin         = top_rank,
                          cmax         = bottom_rank),
              
              # Dates
              dimensions = list(
                
                # # Day 1
                #list(range = c(~max(`12-12`),~min(`12-12`)),
                #     constraintrange = c(0,10),
                #     label   = start_date, values = ~`12-12`,
                #     name    = "Team",
                #     visible = TRUE),
                
                # # Day 2
                # list(range = c(~max(`12-13`),~min(`12-13`)),
                #      label = '12-13', values = ~`12-13`),
                
                # # Day 3
                # list(range = c(~max(`12-14`),~min(`12-14`)),
                #      label = '12-14', values = ~`12-14`),
                
                # # Day 4
                # list(range = c(~max(`12-15`),~min(`12-15`)),
                #      label = '12-15', values = ~`12-15`),
                
                # # Day 5
                # list(range = c(~max(`12-16`),~min(`12-16`)),
                #      label = '12-16', values = ~`12-16`),
                
                # # Day 6
                # list(range = c(~max(`12-17`),~min(`12-17`)),
                #      label = '12-17', values = ~`12-17`),
                
                # # Day 7
                #list(range = c(~max(`12-19`),~min(`12-19`)),
                #     label = '12-19', values = ~`12-19`)
                
                # # Day 8
                # list(range = c(~max(`12-20`),~min(`12-20`)),
                #      label = '12-20', values = ~`12-20`),
                
                # # Day 9
                # list(range = c(~max(`12-21`),~min(`12-21`)),
                #      label = '12-21', values = ~`12-21`),
                
                # # Day 10
                # list(range = c(~max(`12-22`),~min(`12-22`)),
                #      label = '12-22', values = ~`12-22`),
                
                # # Day 11
                #list(range = c(~max(`12-23`),~min(`12-23`)),
                #     label = '12-23', values = ~`12-23`)
                
                # # Day 12
                #list(range = c(~max(`12-24`),~min(`12-24`)),
                #     label = '12-24', values = ~`12-24`)
                
                # # Day 13
                # list(range = c(~max(`12-26`),~min(`12-26`)),
                #      label = '12-26', values = ~`12-26`),
                
                # # Day 14
                # list(range = c(~max(`12-27`),~min(`12-27`)),
                #      label = '12-27', values = ~`12-27`),
                
                # # Day 15
                # list(range = c(~max(`12-28`),~min(`12-28`)),
                #      label = '12-28', values = ~`12-28`),
                
                # # Day 16
                #list(range = c(~max(`12-29`),~min(`12-29`)),
                #     label   = '12-29', values = ~`12-29`),
                
                # # Day 17
                #list(range = c(~max(`12-30`),~min(`12-30`)),
                #     label   = '12-30', values = ~`12-30`),
                
                # # Day 18
                #list(range = c(~max(`12-31`),~min(`12-31`)),
                #     label = '12-31', values = ~`12-31`)
                
                # # Day 19
                # list(range = c(~max(`1-02`),~min(`1-02`)),
                #      label = '1-02', values = ~`1-02`),
                
                # # Day 20
                # list(range = c(~max(`1-03`),~min(`1-03`)),
                #      label = '1-03', values = ~`1-03`),
                
                # # Day 21
                # list(range = c(~max(`1-04`),~min(`1-04`)),
                #      label = '1-04', values = ~`1-04`),
                
                # # Day 22
                #list(range = c(~max(`1-05`),~min(`1-05`)),
                #     label = '1-05', values = ~`1-05`),
                
                # # Day 23
                # list(range = c(~max(`1-06`),~min(`1-06`)),
                #      label = '1-06', values = ~`1-06`),
                
                # # Day 24
                # list(range = c(~max(`1-07`),~min(`1-07`)),
                #      label = '1-07', values = ~`1-07`),
                
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