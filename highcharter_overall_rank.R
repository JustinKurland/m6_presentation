# Libraries ----
suppressPackageStartupMessages({
    if(!require(tidyverse)) install.packages('tidyverse')
    library(tidyverse)
    if(!require(lubridate)) install.packages('lubridate')
    library(lubridate)
    if(!require(highcharter)) install.packages('highcharter')
    library(highcharter)
})


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
    select(MD, "Overall Rank (OR)", Team) %>%
    pivot_wider(names_from = MD, values_from = "Overall Rank (OR)") %>%
    replace(is.na(.), 175)


df2 <- df %>%
    asplit(1) %>%
    imap(~list(
        name = str_glue(.x[1]),
        data = as.numeric(.x[2:length(df)]),
        color = "#002D62",
        marker = FALSE
    ))


yAxis_lst <- rep(
    list(
        list(
            labels = list(style = list(color    = "#CE1126",
                                       fontSize = '14px')),
            showFirstLabel = FALSE,
            showLastLabel  = FALSE,
            reversed       = TRUE
        )
    ),
    length(df)
)


hc <-
    highchart(hc_opts = list(yAxis = yAxis_lst)) %>%
    hc_title(
        text = "Daily Rank: Overall Rank (OR)"
    ) %>%
    hc_subtitle(
        text = "Round 1 (March 17 - April 4)"
    ) %>%
    hc_caption(
        text = "This chart leverages daily pulls of the <i>M6 Competition Leaderboard</i>.",
        useHTML = TRUE
    ) %>%
    hc_credits(
        text = "Chart created using R and highcharter",
        href = "http://jkunst.com/highcharter",
        enabled = TRUE
    ) %>%
    hc_plotOptions(series = list(label = list(enabled = FALSE))) %>%
    hc_chart(parallelCoordinates = TRUE,
             type                = "spline",
             borderColor         = '#002D62',
             borderRadius        = 2,
             borderWidth         = 4 ,
             backgroundColor     = "transparent"
    ) %>%
    hc_xAxis(categories = names(df)[2:length(df)]) %>%
    hc_add_series_list(df2)

hc
