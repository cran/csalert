
# gen_csfmt_rts_baseline_data <- function(start_date,
#                                          end_date){
#
#   start_date <- as.Date(start_date)
#   end_date <- as.Date(end_date)
#
#       d <- cstidy::csfmt_rts_data_v1(data.table(
#         location_code = "norge",
#         date = seq.Date(start_date,end_date,by="day"),
#         age = "total",
#         sex = "total",
#         border = 2020,
#         granularity_time="day"
#       ))
#       d[, time:=1:.N]
#
#       d[, wday:=lubridate::wday(date)]
#
#   return(d)
# }

periodic_pattern <- function(n_p = 2,
                             g1 = 0.8,
                             g2 = 0.4,
                             s = 29,
                             p = 52*7,
                             t = 1:nrow(d)){

  d <- NULL

        l <-  1:n_p

        c <- rep(0, length(t))

        for(i in 1:length(t)){

            c[i] <- sum(g1*cos((2*pi*l*(t[i]+s))/(p))+g2*sin((2*pi*l*(t[i]+s))/(p)))
        }

        return(c)
}

#' Simulate baseline data ----
#' Simulation of baseline data.
#'
#' @description
#' This function simulates a time series of daily counts in the absence of outbreaks. Data is simulated using a poisson/negative binomial model as described in
#' Noufaily et al. (2019).
#' Properties of time series such as frequency of baseline observations, trend, seasonal and weekly pattern can be specified in the simulation.
#'
#' @param start_date Starting date of the simulation period.
#'   Date is in the format of 'yyyy-mm-dd'.
#'
#' @param end_date Ending date of the simulation period.
#'   Date is in the format of 'yyyy-mm-dd'.
#'
#' @param seasonal_pattern_n Number of seasonal patterns. For no seasonal pattern seasonal_pattern_n = 0. Seasonal_pattern_n = 1 represents annual pattern. Seasonal_pattern_n = 2 indicates biannual pattern.
#'
#' @param weekly_pattern_n Number of weekly patterns. For no specific weekly pattern, weekly_pattern_n = 0. Weekly_pattern_n = 1 represents one weekly peak.
#'
#' @param alpha The parameter is used to specify the baseline frequencies of reports
#' @param beta The parameter is used to specify to specify linear trend
#' @param gamma_1 The parameter is used to specify the seasonal pattern
#' @param gamma_2 The parameter is used to specify the seasonal pattern
#' @param gamma_3 The parameter is used to specify day-of-the week pattern
#' @param gamma_4 The parameter is used to specify day-of-the week pattern
#' @param phi Dispersion parameter. If phi =0, a Poisson model is used to simulate baseline data.
#' @param shift_1 Horizontal shift parameter to help control over week/month peaks.
#'
#' @return
#' A csfmt_rts_data_v1, data.table containing a time series of counts
#'
#' \describe{
#'   \item{wday}{day-of-the week}
#'   \item{n}{cases}
#' }
#'
#' @export
#' @examples
#' baseline  <- simulate_baseline_data(
#' start_date = as.Date("2012-01-01"),
#' end_date = as.Date("2019-12-31"),
#' seasonal_pattern_n = 1,
#' weekly_pattern_n = 1,
#' alpha = 3,
#' beta = 0,
#' gamma_1 = 0.8,
#' gamma_2 = 0.6,
#' gamma_3 = 0.8,
#' gamma_4 = 0.4,
#' phi = 4,
#' shift_1 = 29 )


simulate_baseline_data <-  function(start_date,
                                    end_date,
                                    seasonal_pattern_n,
                                    weekly_pattern_n,
                                    alpha,
                                    beta,
                                    gamma_1,
                                    gamma_2,
                                    gamma_3,
                                    gamma_4,
                                    phi,
                                    shift_1){

  time <- NULL
  trend <- NULL
  seasonal_pattern <- NULL
  weekly_pattern <- NULL
  n <- NULL

  # d <- csalert::gen_csfmt_rts_baseline_data(start_date, end_date)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  d <- cstidy::csfmt_rts_data_v1(data.table(
        location_code = "norge",
        date = seq.Date(start_date,end_date,by="day"),
        age = "total",
        sex = "total",
        border = 2020,
        granularity_time="day"
  ))

  d[, time:=1:.N]
  d[, wday:=lubridate::wday(date)]
  t <- 1:nrow(d)
  d[, phi:= phi]

  if (seasonal_pattern_n == 0 & weekly_pattern_n == 0){

            d[, mu := exp(alpha + (beta * time))]

  } else {

        if (seasonal_pattern_n == 0){

            wp_n = weekly_pattern_n

            d[, trend := alpha + (beta * (time + shift_1))]
            d[, seasonal_pattern := NA]
            d[, weekly_pattern := periodic_pattern(wp_n, gamma_3,gamma_4, shift_1,7,time)]

            d[, mu:= exp(trend + weekly_pattern)]

        } else {
            wp_n = weekly_pattern_n
            sp_n = seasonal_pattern_n

            d[, trend := alpha + (beta * (time + shift_1))]
            d[, seasonal_pattern := periodic_pattern(sp_n,gamma_1,gamma_2,shift_1,52*7,t)]
            d[, weekly_pattern := periodic_pattern(wp_n, gamma_3,gamma_4, shift_1,7,t)]

            d[, mu:= exp(trend + seasonal_pattern + weekly_pattern)]

            }

  }


  mu <- d$mu

  if (phi==1) {
    d[, n := stats::rpois(.N, lambda=mu)]
  } else {
    prob <- 1/phi
    size <- mu/(phi-1)
    d[, n := stats::rnbinom(.N,size=size,prob=prob)]
  }


  return(d)
}




#' Simulate seasonal outbreaks ----
#' @description
#' Simulation of seasonal outbreaks for syndromes/diseases that follows seasonal trends.
#' Seasonal outbreaks are more variable both in size and timing than  seasonal patterns.
#' The number of seasonal outbreaks occur in a year are defined by n_season_outbreak.
#' The parameters week_season_start and week_season_end define the season window.
#' The start of the seasonal outbreak is drawn from the season window weeks, with higher probability of outbreak occurs around the peak of the season (week_season_peak).
#' The seasonal outbreak size (excess number of cases that occurs during the outbreak) is simulated using a poisson distribution as described in Noufaily et al. (2019).
#'
#' @param data
#' A csfmt_rds data object
#' @param week_season_start Starting season week number
#' @param week_season_peak Peak of the season week number
#' @param week_season_end Ending season week number
#' @param n_season_outbreak Number of seasonal outbreaks to be simulated
#' @param m Parameter to determine the size of the outbreak (m times the standard deviation of the baseline count at the starting day of the seasonal outbreak)

#' @return
#' A csfmt_rts_data_v1, data.table
#'
#' @export


simulate_seasonal_outbreak_data <-  function(data,
                                             week_season_start = 40,
                                             week_season_peak = 4,
                                             week_season_end = 20,
                                             n_season_outbreak = 1,
                                             m=50){

  mu <- NULL
  phi <- NULL
  weight <- NULL
  seasonal_outbreak <- NULL
  seasonal_outbreak_n <- NULL
  seasonal_outbreak_rw <- NULL
  isoyearweek <- NULL
  years_out <- NULL
  seasonal_outbreak_n_rw <- NULL
  n <- NULL

  d <- copy(data)
  N <- nrow(d)

  d[, sd:= sqrt(mu*phi)]

  ## wdays reweight ## should this be part of parameters?
  d[wday==1, weight:=0.5]
  d[wday==2, weight:=1]
  d[wday==3, weight:=1]
  d[wday==4, weight:=1]
  d[wday==5, weight:=1]
  d[wday==6, weight:=2]
  d[wday==7, weight:=2]


  ## select year were there is seasonal outbreak

  n_year <- length(unique(d$calyear))
  years <- sort(unique(d$calyear))[1:(n_year-1)]

  # random sampling of numbers of years with seasonal outbreak
  n_out <- sample(1:length(years),1)
  years_out <- sort(sample(years,n_out,replace=F))

  print(years_out)

  d[, seasonal_outbreak := 0]
  d[, seasonal_outbreak_n:=0]
  d[, seasonal_outbreak_n_rw:=0]

  for (y in years_out) {

    # set.seed(y)

    wtime <- c(paste(y,c(week_season_start:52),sep="-"), paste(y+1,stringr::str_pad(c(1:week_season_end),2,pad="0"),sep="-"))
    time <- d[isoyearweek %in% wtime]$time

    # probability of outbreak start around the peak of seasoanl
    start_seasonal_outbreak <- sample(time, n_season_outbreak,replace = FALSE,prob=abs(stats::rnorm(length(time))))

    # number of cases for outbreat
      n_cases_outbreak <- rep(0, n_season_outbreak)

      size_outbreak <- 1
      sou=1

      for (i in 1: n_season_outbreak) {
          while(size_outbreak < 2){
            set.seed(sou)
            sd <- d[time == start_seasonal_outbreak[i]]$sd
            size_outbreak=stats::rpois(1,sd*m*10)
            sou=sou+1
          }

          n_cases_outbreak[i]  <- size_outbreak


        # Cases are distributed from the start of outbreak using a log normal distribution

        outbreak <- stats::rlnorm(n_cases_outbreak[i], meanlog = 0, sdlog = 0.5)

        h <- graphics::hist(outbreak,breaks=seq(0,ceiling(max(outbreak)),0.1),plot=FALSE)

        outbreak_n <- h$counts
        duration <-start_seasonal_outbreak[i]:(start_seasonal_outbreak[i] + length(outbreak_n)-1)

        d[time %in% duration, seasonal_outbreak_n := outbreak_n]
        d[time %in% duration, seasonal_outbreak_n_rw := outbreak_n * weight]
        d[time %in% duration, seasonal_outbreak:=1]


      }

  }

  d[, n := n + seasonal_outbreak_n_rw]
  return(d)
}



#' Simulate spiked outbreaks ----
#' @description

#' Simulation of spiked outbreak as described in Noufaily et al. (2019). The method for simulating spiked outbreak is similar to
#' seasonal outbreaks simulation but they are shorter in duration and are added only the last year of data (prediction data).
#' Spiked outbreaks can start at any week during the prediction data
#'
#' @param data
#' A csfmt_rds data object
#' @param n_sp_outbreak Number of spiked outbreaks to be simulated
#' @param m Parameter to determine the size of the outbreak (m times the standard deviation of the baseline count at the starting day of the seasonal outbreak)
#'
#' @return
#' A csfmt_rts_data_v1, data.table
#'
#' @export


simulate_spike_outbreak_data <-  function(data,
                                          n_sp_outbreak = 1,
                                          m){

  mu <- NULL
  phi <- NULL
  weight <- NULL
  sp_outbreak_n <- NULL
  sp_outbreak_n_rw <- NULL
  n <- NULL
  sp_outbreak <- NULL
  n <- NULL

  d <- copy(data)

  N <- nrow(d)
  d[, sd:= sqrt(mu*phi)]

  d[wday==1, weight:=0.5]
  d[wday==2, weight:=1]
  d[wday==3, weight:=1]
  d[wday==4, weight:=1]
  d[wday==5, weight:=1]
  d[wday==6, weight:=2]
  d[wday==7, weight:=2]


    wtime <- (nrow(d) - 49*7) : nrow(d)

    time <- d[time %in% wtime]$time

    startoutbk <- sample(time, n_sp_outbreak,replace = FALSE)

    # OUTBREAK SIZE OF CASES

    n_cases_outbreak=rep(0,n_sp_outbreak)
    soutbk=1
    sou=1

    d[, sp_outbreak_n:=0]
    d[, sp_outbreak_n_rw:=0]

    for (i in 1:n_sp_outbreak) {

        while(soutbk<2){
          set.seed(sou)
          sd <- d[time == startoutbk[i]]$sd
          soutbk=stats::rpois(1,sd*m*10)
          sou=sou+1
        }

        n_cases_outbreak[i]=soutbk


        # Cases are distributed from the start of outbreak using a log normal distribution
        outbreak <-stats::rlnorm(n_cases_outbreak[i], meanlog = 0, sdlog = 0.5)
        h <- graphics::hist(outbreak,breaks=seq(0,ceiling(max(outbreak)),0.2),plot=FALSE)
        outbreak_n <- h$counts
        duration <-startoutbk[i]:(startoutbk[i]+length(outbreak_n)-1)

        d[time %in% duration, sp_outbreak_n := outbreak_n]
        d[time %in% duration, sp_outbreak_n_rw := outbreak_n * weight]
        d[time %in% duration, sp_outbreak:=2]


    }

    d[is.na(sp_outbreak), sp_outbreak:=0]
    d[is.na(sp_outbreak_n), sp_outbreak_n:=0]
    d[is.na(sp_outbreak_n_rw), sp_outbreak_n_rw:=0]



    d[, n := n + sp_outbreak_n]


    return(d)
}




#' Holiday effect ----
#' @description
#' The effect of public holiday on a time series of daily counts
#' @param data
#' A csfmt_rds data object
#' @param holiday_data dates
#' @param holiday_effect Ending date of the simulation period.
#'
#' @return
#' A csfmt_rts_data_v1, data.table containing
#'
#' @export


add_holiday_effect <-  function(data,
                                holiday_data,
                                holiday_effect = 2){

  holiday <- NULL
  is_holiday <- NULL
  n <- NULL
  d <- NULL

  d <- copy(data)

  d[
    holiday_data,
    on=c("date"),
    holiday := is_holiday
  ]

  d[holiday==T, n:= n*holiday_effect]


  return(d)
}




# run_simulation <- function(start_date,
#                            end_date,
#                            seasonal_pattern_n,
#                            weekly_pattern_n,
#                            week_season_start,
#                            week_season_peak ,
#                            week_season_end,
#                            n_season_outbreak,
#                            num_sp_outbreak,
#                            baseline_param_list = list(alpha, beta, gamma1,gamma3,gamma4,phi,shift),
#                            outbreak_param_list = list(m),
#                            nsim) {
#
#
#     retval <- list()
#
#     for (i in 1:(nsim)) {
#
#           data <- simulate_baseline_data(start_date,
#                                      end_date,
#                                      seasonal_pattern_n,
#                                      weekly_pattern_n,
#                                      baseline_param_list)
#       if (seasonal_pattern_n!=0) {
#
#           data <- simulate_seasonal_outbreak_data(data,
#                                               week_season_start,
#                                               week_season_peak ,
#                                               week_season_end,
#                                               n_season_outbreak,
#                                               baseline_param_list,
#                                               outbreak_param_list)
#       }
#
#       data <- simulate_spike_outbreak_data(data,
#                                            num_sp_outbreak,
#                                            baseline_param_list,
#                                            outbreak_param_list)
#
#       retval[[i]] <-  data
#
#
#     }
#
#     return(retval)
#
# }

# sim <- run_simulation(nsim=100,
#                start_date = as.Date("2012-01-01"),
#                end_date = as.Date("2019-12-31"),
#                seasonal_pattern_n = 1,
#                weekly_pattern_n = 1,
#                week_season_start = 40,
#                week_season_peak = 4,
#                week_season_end = 20,
#                n_season_outbreak = 1,
#                num_sp_outbreak = 1,
#                baseline_param_list = list(alpha = 3, beta = 0,gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.8, gamma4 = 0.4, phi = 4,shift = 29 ),
#                outbreak_param_list = list(m=2)
# )
