## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)

## -----------------------------------------------------------------------------
d_hosp <- cstidy::nor_covid19_icu_and_hospitalization_csfmt_rts_v1
d_hosp

## -----------------------------------------------------------------------------
d_hosp_weekly <- d_hosp[granularity_time=="isoyearweek"]

res <- csalert::short_term_trend(
  d_hosp_weekly, 
  numerator = "hospitalization_with_covid19_as_primary_cause_n",
  trend_isoyearweeks = 6,
  remove_last_isoyearweeks = 1
)

# create the trend label
res[, hospitalization_with_covid19_as_primary_cause_trend0_42_status := factor(
  hospitalization_with_covid19_as_primary_cause_trend0_42_status,
  levels = c("training","forecast","decreasing", "null", "increasing"),
  labels = c("Training","Forecast","Decreasing", "Null", "Increasing")
)]

colnames(res)

## -----------------------------------------------------------------------------
# check some columns 
res[
  ,
  .(
    date, 
    hospitalization_with_covid19_as_primary_cause_n, 
    hospitalization_with_covid19_as_primary_cause_forecasted_n,
    hospitalization_with_covid19_as_primary_cause_trend0_42_status
  )
]

## -----------------------------------------------------------------------------
q <- ggplot(
  res, 
  aes(
    x = isoyearweek, 
    y = hospitalization_with_covid19_as_primary_cause_forecasted_n,
    group = 1
  )
)
q <- q + geom_col(mapping = aes(fill = hospitalization_with_covid19_as_primary_cause_trend0_42_status))
q <- q + geom_errorbar(
  mapping = aes(
    ymin = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q02x5_n,
    ymax = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q97x5_n
  )
)
q <- q + scale_y_continuous("Weekly hospitalization with Covid-19 as primary cause", expand = c(0, 0.1))
q <- q + scale_x_discrete("Isoyearweek")
q <- q + expand_limits(y=0)
q <- q + scale_fill_brewer("6 week trend", palette = "Set1")
q

## -----------------------------------------------------------------------------
d_hosp_daily <- d_hosp[granularity_time=="date"]

res <- csalert::short_term_trend(
  d_hosp_daily, 
  numerator = "hospitalization_with_covid19_as_primary_cause_n",
  trend_dates = 28,
  remove_last_dates = 7
)
res[, hospitalization_with_covid19_as_primary_cause_trend0_28_status := factor(
  hospitalization_with_covid19_as_primary_cause_trend0_28_status,
  levels = c("training","forecast","decreasing", "null", "increasing"),
  labels = c("Training","Forecast","Decreasing", "Null", "Increasing")
)]


## -----------------------------------------------------------------------------
q <- ggplot(
  data = res, 
  mapping = aes(
    x = date, 
    y = hospitalization_with_covid19_as_primary_cause_forecasted_n, 
    fill = hospitalization_with_covid19_as_primary_cause_trend0_28_status
  )
)
q <- q + geom_col(alpha = 0) # necessary to get legend to be in right order
q <- q + geom_col(
  data = res[hospitalization_with_covid19_as_primary_cause_trend0_28_status!="Forecast"]
  )
q <- q + geom_ribbon(
  data = res[hospitalization_with_covid19_as_primary_cause_trend0_28_status=="Forecast"], 
  mapping = aes(
    ymin = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q02x5_n,
    ymax = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q97x5_n
  ),
  alpha = 0.75
)
q <- q + scale_y_continuous("Daily hospitalization with Covid-19 as primary cause", expand = c(0, 0.1))
q <- q + scale_x_date("Date", date_breaks = "6 months", date_labels = "%y-%m-%d")
q <- q + expand_limits(y=0)

q <- q + scale_fill_brewer("28 days trend", palette = "Set1")
q

## -----------------------------------------------------------------------------
q <- ggplot(
  res, 
  aes(
    x = date, 
    y = hospitalization_with_covid19_as_primary_cause_doublingdays0_28
  )
)
q <- q + geom_rect(
  aes(
    xmin = date-1, 
    xmax=date, 
    ymin = 1,
    ymax = Inf, 
    fill = hospitalization_with_covid19_as_primary_cause_trend0_28_status
  ), 
  alpha = 0.5
)
q <- q + geom_line(lwd = 1)
q <- q + scale_y_continuous(trans = "log10", expand = c(0, 0.1))
q <- q + scale_fill_brewer(NULL, palette = "Set1")
q

## -----------------------------------------------------------------------------
d <- cstidy::nor_covid19_cases_by_time_location_csfmt_rts_v1[
  granularity_time == "isoyearweek" & 
  granularity_geo == "county"
]

trend <- csalert::short_term_trend(
  d,
  numerator = "covid19_cases_testdate_n",
  trend_isoyearweeks = 6,
  remove_last_isoyearweeks = 1
)

print(trend)

## -----------------------------------------------------------------------------
pd <- copy(csmaps::nor_county_map_b2020_split_dt)
pd[
  trend[isoyearweek == "2021-44"],
  on = c("location_code"),
  covid19_cases_testdate_trend0_42_status := covid19_cases_testdate_trend0_42_status
]

# plot map
q <- ggplot()
q <- q + geom_polygon(
  data = pd,
  mapping = aes(x = long, y = lat, group = group,fill=covid19_cases_testdate_trend0_42_status),
  color="black",
  linewidth = 0.2
)
q <- q + coord_quickmap()
q <- q + theme_void()
q <- q + labs(title="MSIS cases per 100k population for week 2021-44")
q <- q + scale_fill_brewer("Covid trends", palette = "Set1", direction = -1)
q

