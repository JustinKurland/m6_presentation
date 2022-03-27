# Libraries ----
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(plotly)))
suppressWarnings(suppressMessages(library(widgetframe)))

# Read and Wrangle ----
df <- read.csv("M6_Leaderboard.csv") %>%
    as_tibble() %>%
    mutate(Date = as.Date(mdy(Date))) %>%
    arrange(Date, Position) %>%
    setNames(c("Date","Position" ,"Team","Overall Rank (OR)", "Performance of Forecasts (RPS)", "Rank (Forecasts)", "Performance of Decisions (IR)", "Rank (Decisions)")) %>%
    mutate(Month = month(Date)) %>%
    mutate(Day   = day(Date)) %>%
    mutate(MD    = str_c(Month, "-", Day)) %>%
    select(MD, Position, "Overall Rank (OR)", Team, "Rank (Forecasts)", "Rank (Decisions)") %>%
    mutate(Position = as.integer(Position)) %>%
    mutate(`Rank (Forecasts)` = as.integer(`Rank (Forecasts)`)) %>%
    mutate(`Rank (Decisions)` = as.integer(`Rank (Decisions)`))


# Parallel Coord Function ----
plot_parallel_coord <- function(data,
                                kpi          = "Position",
                                start_date   = "3-17",
                                colorscale   = 'RdBu',
                                showscale    = TRUE,
                                reversescale = FALSE,
                                top_rank     = 0,
                                bottom_rank  = 176,
                                plot_width   = 700,
                                plot_height  = 500
) {

    # Import Data
    g <- data %>%

        # Select Columns of Interest
        select(MD, kpi, Team) %>%

        # Pivot Wider
        pivot_wider(names_from = MD, values_from = kpi) %>%

        # Replace NAs with Max Rank
        replace(is.na(.), 176) %>%

        # Input Width and Height
        plot_ly(width = plot_width, height = plot_height) %>%

        # Add Trace
        add_trace(type = 'parcoords',
                  line = list(color        = ~`3-17`,
                              colorscale   = colorscale,
                              showscale    = showscale,
                              reversescale = reversescale,
                              cmin         = top_rank,
                              cmax         = bottom_rank),

                  # Dates
                  dimensions = list(

                      # Day 1
                      list(range = c(~max(`3-17`),~min(`3-17`)),
                           constraintrange = c(0,10),
                           label   = start_date, values = ~`3-17`,
                           name    = "Team",
                           visible = TRUE),

                      # Day 2
                      list(range = c(~max(`3-18`),~min(`3-18`)),
                           label = '3-18', values = ~`3-18`),

                      # Day 3
                      list(range = c(~max(`3-19`),~min(`3-19`)),
                           label = '3-19', values = ~`3-19`),

                      # Day 4
                      list(range = c(~max(`3-21`),~min(`3-21`)),
                           label = '3-21', values = ~`3-21`),

                      # Day 5
                      list(range = c(~max(`3-22`),~min(`3-22`)),
                           label = '3-22', values = ~`3-22`),

                      # Day 6
                      list(range = c(~max(`3-23`),~min(`3-23`)),
                           label = '3-23', values = ~`3-23`),

                      # Day 7
                      list(range = c(~max(`3-24`),~min(`3-24`)),
                           label = '3-24', values = ~`3-24`),

                      # Day 8
                      list(range = c(~max(`3-25`),~min(`3-25`)),
                           label = '3-25', values = ~`3-25`),

                      # Day 9
                      list(range = c(~max(`3-26`),~min(`3-26`)),
                           label = '3-26', values = ~`3-26`)

                      # # Day 10
                      # list(range = c(~max(`3-28`),~min(`3-28`)),
                      #      label = '3-21', values = ~`3-21`),
                      #
                      # # Day 11
                      # list(range = c(~max(`3-29`),~min(`3-29`)),
                      #      label = '3-29', values = ~`3-30`)

                      # # Day 12
                      # list(range = c(~max(`3-30`),~min(`3-30`)),
                      #      label = '3-30', values = ~`3-30`),
                      #
                      # # Day 13
                      # list(range = c(~max(`3-31`),~min(`3-31`)),
                      #      label = '3-31', values = ~`3-31`)

                      # # Day 14
                      # list(range = c(~max(`4-01`),~min(`4-01`)),
                      #      label = '3-25', values = ~`3-26`),
                      #
                      # # Day 15
                      # list(range = c(~max(`4-02`),~min(`4-02`)),
                      #      label = '4-02', values = ~`4-02`)

                  )
        ) %>%

        layout(title = str_glue("Daily Rank: {kpi}"))

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

