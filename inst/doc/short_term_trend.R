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
res[, hospitalization_with_covid19_as_primary_cause_trend0_41_status := factor(
  hospitalization_with_covid19_as_primary_cause_trend0_41_status,
  levels = c("training","forecast","notincreasing", "increasing"),
  labels = c("Training","Forecast","Not increasin", "Increasing")
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
    hospitalization_with_covid19_as_primary_cause_trend0_41_status
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
q <- q + geom_col(mapping = aes(fill = hospitalization_with_covid19_as_primary_cause_trend0_41_status))
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
shape_adjustment_factor <- max(res$hospitalization_with_covid19_as_primary_cause_forecasted_n)*0.01
q <- ggplot(
  res, 
  aes(
    x = isoyearweek, 
    y = hospitalization_with_covid19_as_primary_cause_forecasted_n,
    group = 1
  )
)
q <- q + geom_col()
q <- q + geom_point(mapping = aes(
  y = hospitalization_with_covid19_as_primary_cause_forecasted_n + shape_adjustment_factor,
  shape = hospitalization_with_covid19_as_primary_cause_trend0_41_status
))
q <- q + geom_errorbar(
  mapping = aes(
    ymin = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q02x5_n,
    ymax = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q97x5_n
  )
)
q <- q + scale_y_continuous("Weekly hospitalization with Covid-19 as primary cause", expand = c(0, 0.1))
q <- q + scale_x_discrete("Isoyearweek")
q <- q + expand_limits(y=0)
q <- q + scale_shape_manual("6 week trend", values = c("Increasing" = 17, "Decreasing" = 6))
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
  covid19_cases_testdate_trend0_41_status := covid19_cases_testdate_trend0_41_status
]

# plot map
q <- ggplot()
q <- q + geom_polygon(
  data = pd,
  mapping = aes(x = long, y = lat, group = group,fill=covid19_cases_testdate_trend0_41_status),
  color="black",
  linewidth = 0.2
)
q <- q + coord_quickmap()
q <- q + theme_void()
q <- q + labs(title="MSIS cases per 100k population for week 2021-44")
q <- q + scale_fill_brewer("Covid trends", palette = "Set1", direction = -1)
q

