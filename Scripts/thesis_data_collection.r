##############################################################################################################################################################################

############################################################################## Collect Data ##############################################################################

##############################################################################################################################################################################


pacman::p_load(dplyr, tidyr, econdatar, ggplot2, conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

##############################################################################################################################################################################

######################################################################### Historical Sample (1992 - 2012) ####################################################################

##############################################################################################################################################################################

####
## PPI
####

## PI000002 = All Groups
## PI200002 = Manufacturing

ppi_hist <- tibble(time_period = as.Date(character()),
            ppi_all = numeric(), 
            ppi_manuf = numeric()
)

periods <- c("1990", "2000")
codes <- c("PI000002", "PI200002")

for (period in periods){
        ppi_temp_i <- tibble(time_period = as.Date(character())
)
        for (code in codes){
            if (code == "PI000002"){
                name <- "ppi_all"
            } else {
                name <- "ppi_manuf"
            }
            xlfile <- paste0("ppi_", period, ".xlsx")

            temp_ppi <- readxl::read_excel(here::here("data", "PPI DATA", xlfile)) %>%
                filter(H03 == code) %>%
                select(-c(H01, H02, H17, H25)) %>%
                pivot_longer(-c("H03", "H04", "H05", "H18"), names_to = "period", values_to = name) %>%
                mutate(time_period = as.Date(paste0(stringr::str_sub(period, 5, 8), "-", stringr::str_sub(period, 3, 4), "-01"))) %>%
                select(time_period, !!sym(name)) %>%
                mutate(!!sym(name) := as.numeric(!!sym(name))) %>%
                arrange(time_period)

            ppi_temp_i <- full_join(ppi_temp_i, temp_ppi, by = "time_period")
        }
            ppi_hist <- bind_rows(ppi_hist, ppi_temp_i)
}


ppi_hist <- ppi_hist %>%
    mutate(ppi_all = log(ppi_all) - log(lag(ppi_all, n = 1)), 
    ppi_manuf = log(ppi_manuf) - log(lag(ppi_manuf, n = 1)))
    

####
## IMPORT PRICES
####

## PI000002 = All Groups
## PI200002 = Manufacturing

m <- tibble(time_period = as.Date(character()),
            m_all = numeric(), 
            m_manuf = numeric()
)

periods <- c("1990", "2000")
codes <- c("PI000004", "PI200004")
for (period in periods){
       m_temp_i <- tibble(time_period = as.Date(character())
)
        for (code in codes){
            if (code == "PI000004"){
                name <- "m_all"
            } else {
                name <- "m_manuf"
            }
            xlfile <- paste0("ppi_", period, ".xlsx")

            temp_m <- readxl::read_excel(here::here("data", "PPI DATA", xlfile)) %>%
                filter(H03 == code) %>%
                select(-c(H01, H02, H17, H25)) %>%
                pivot_longer(-c("H03", "H04", "H05", "H18"), names_to = "period", values_to = name) %>%
                mutate(time_period = as.Date(paste0(stringr::str_sub(period, 5, 8), "-", stringr::str_sub(period, 3, 4), "-01"))) %>%
                select(time_period, !!sym(name)) %>%
                mutate(!!sym(name) := as.numeric(!!sym(name))) %>%
                arrange(time_period)

            m_temp_i <- full_join(m_temp_i, temp_m, by = "time_period")
        }
            m <- bind_rows(m, m_temp_i)
}

m <- m %>%
    mutate(m_all = log(m_all) - log(lag(m_all, n = 1)), 
            m_manuf = log(m_manuf) - log(lag(m_manuf, n = 1)))

####
## CPI
####

## Headline

cpi <- read_dataset(id = "CPI_ANL_SERIES", 
                        series_key = "CPS00000") %>%
                        as_tibble() %>%
                        rename(value = "CPS00000") %>%
                        filter(time_period >= as.Date("1990-01-01") & time_period <= as.Date("2012-12-01")) %>%
                        mutate(cpi = log(value) - log(lag(value, n = 1)))

####
## Output Gap Indicators
####
##BCI BER

bci <- readxl::read_excel(here::here("data", "BCI DATA", "BCI_February_2026.xls")) %>%
    rename(time_period = "...1", 
            leading = "Leading\n indicator", 
            lagging = "Lagging\n indicator",
            coincident = "Coincident\n indicator") %>%
    mutate(time_period = lubridate::floor_date(lubridate::as_date(time_period), "month"),,
    leading_lagged = lag(leading, n = 1), 
    coincident_lagged = lag(coincident, n = 1),
    across(c(leading, lagging, coincident, leading_lagged, coincident_lagged), as.numeric), 
    leading_detrended = leading - leading_lagged,
    coincident_detrended = coincident - coincident_lagged) %>%
    filter(time_period >= as.Date("1990-01-01") & time_period <= as.Date("2012-12-31")) %>%
    group_by(time_period)

hp <-  mFilter::hpfilter(bci$coincident, freq = 14400, type = "lambda")

bci <- bci %>%
    ungroup() %>%
    mutate(outputgap_bci = hp$cycle)

bci_ts <- ts(bci %>% select(!time_period), start = c(1990, 1), frequency = 12)

bci_aligned <- window(bci_ts[, "coincident_detrended"],
                      start = c(1990, 1),
                      end   = c(2012, 12))


##SARB 

sarb_gdp <- read_dataset(id = "QB_NATLACC", 
                        series_key = "KBP6006D.Q.R.S.LA") %>%
                        as_tibble() %>%
                        rename(gdp = "KBP6006D.Q.R.S.LA") %>%
                        mutate(lgdp = log(gdp)) %>%
                        filter(time_period >= as.Date("1990-01-01") & time_period <= as.Date("2012-12-01")) %>%
                        mutate(detrended_gdp = gdp - lag(gdp)) 

sarb_gdp_ts <- ts(sarb_gdp %>% select(lgdp), 
    start = c(1990, 1), 
    frequency = 4)

model_dc <- tempdisagg::td(sarb_gdp_ts ~ 1, 
    to = 12,
     method = "denton-cholette")

gdp_monthly_dc <- predict(model_dc)

gdp_monthly_dc <- data.frame(
  time_period = zoo::as.Date(zoo::as.yearmon(time(gdp_monthly_dc))),
  dc_monthly = as.numeric(gdp_monthly_dc)
)

hp_dc <-  mFilter::hpfilter(gdp_monthly_dc$dc_monthly, freq = 14400, type = "lambda")

gdp_monthly_dc <- gdp_monthly_dc %>%
  mutate(outputgap_dc = hp_dc$cycle)

model_cl <- tempdisagg::td(sarb_gdp_ts ~ bci_aligned,
    to = 12,
    method = "chow-lin-maxlog")

gdp_monthly_cl <- predict(model_cl)

gdp_monthly_cl <- data.frame(
  time_period = zoo::as.Date(zoo::as.yearmon(time(gdp_monthly_cl))),
  cl_monthly = as.numeric(gdp_monthly_cl)
) 

hp_cl <-  mFilter::hpfilter(gdp_monthly_cl$cl_monthly, freq = 14400, type = "lambda")

gdp_monthly_cl <- gdp_monthly_cl %>%
  mutate(outputgap_cl = hp_cl$cycle)



outputgap <- left_join(bci, left_join(gdp_monthly_cl, gdp_monthly_dc, by = "time_period"), by = "time_period") %>%
    select(time_period, outputgap_bci, outputgap_dc, outputgap_cl)


####
## Exchange Rate Data
####

##USD/ZAR from Fred
usdzar <- fredr::fredr(series_id = "DEXSFUS", 
                    observation_start = as.Date("1990-01-01"), 
                    observation_end = as.Date("2012-12-31"),   
                    frequency = "m") %>%
                    mutate(time_period = as.Date(paste0(stringr::str_sub(as.character(date), 1, 7), "-01"))) %>%
                    select(time_period, value) %>%
                    rename(rate = value) %>%
                    mutate(usdzar_fred = log(rate) - log(lag(rate, n = 1))) 

##How many rands for a dollar, positive value means rand has depreciated

##NEER from SARB
neer <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
    mutate(time_period = lubridate::my(Date),
    neer_sarb = log(Value) - log(lag(Value, n = 1))) %>%
    select(-Date) %>%
    filter(time_period >= as.Date("1990-01-01") & time_period <= as.Date("2013-03-01"))



####
## Brent Crude  Data
####

oil <- fredr::fredr(series_id = "POILBREUSDM", 
                    observation_start = as.Date("1990-01-01"), 
                    observation_end = as.Date("2012-12-31"),   
                    frequency = "m") %>%
                    mutate(time_period = as.Date(paste0(stringr::str_sub(as.character(date), 1, 7), "-01"))) %>%
                    select(time_period, value) %>%
                    mutate(oil = log(value) - log(lag(value, n = 1))) %>%
                    select(-value)

shock <- readxl::read_excel(here::here("data", "oilsupplynews", "oilSupplyNewsShocks_2025M06.xlsx"), sheet = "Monthly") %>%
    select(Date, `Oil supply news shock`) %>%
    mutate(time_period = as.Date(paste0(stringr::str_sub(Date, 1, 4), "-", stringr::str_sub(Date, 6, 7), "-01"), format = "%Y-%m-%d"), oilshock = `Oil supply news shock`) %>%
    select(time_period, oilshock) 

oil <- left_join(oil, shock, by = "time_period")


####

##MERGE DATA


####

historical_sample <- m %>%
  full_join(ppi_hist,        by = "time_period") %>%
  full_join(cpi,        by = "time_period") %>%
  full_join(outputgap, by = "time_period") %>%
  full_join(neer,       by = "time_period") %>%
  full_join(usdzar,     by = "time_period") %>%
  full_join(oil,        by = "time_period") %>%
filter(time_period >= as.Date("1992-01-01") & time_period <= as.Date("2012-12-01"))



filename <- file.path(here::here(), "data", "samples", "historicalsample.csv")
write.csv(historical_sample, filename)



##############################################################################################################################################################################

######################################################################### Latest Sample (2008 - 2026) ####################################################################

##############################################################################################################################################################################

####
## PPI
####

##We need to map intermediate and full onto the historical PI2000002 series in order to find the weights

names <- c("finalmanufgoods_full", "finalmanufgoods_early", "intermediatemanufgoods_full", "intermediatemanufgoods_early")


ppi <- tibble(time_period = as.Date(character(0)))


for (name in names){

    temp_ppi <- readxl::read_excel(here::here("data", "ppi.xlsx"), sheet = "Sheet3") %>%
                    filter(`...1` == name) %>%
                    rename(type = "...1") %>%
                    select(!`...2`) %>%
                    pivot_longer(-type, names_to = "time_period", values_to = name) %>%
                    mutate(time_period = paste0(stringr::str_sub(time_period, 5, 8), "-", stringr::str_sub(time_period, 3, 4), "-01")) %>%
                    select(time_period, !!sym(name)) %>%
                    mutate(!!sym(name) := as.numeric(!!sym(name)), !!sym(name) :=  log(!!sym(name)) - log(lag(!!sym(name), n = 1))) %>%
                    arrange(time_period) %>%
                    mutate(time_period = as.Date(time_period, format = "%Y-%m-%d"))

    ppi <- full_join(ppi, temp_ppi, by = "time_period")
}

####
## Compute weights
####

df <- inner_join(ppi, ppi_hist, by = "time_period") %>%
   select(finalmanufgoods_early, intermediatemanufgoods_early, ppi_manuf)

model <- lm(I(ppi_manuf - intermediatemanufgoods_early) ~ I(finalmanufgoods_early - intermediatemanufgoods_early) - 1, data = df)
p <- coef(model)[[1]]

print(summary(model))
print(p) ## p = 0.660612, so multiply final goods
         ## by 0.660612 and intermediate goods by 1 - 0.660612. 

ppi <- ppi %>%
        mutate(ppi_manuf = ifelse(!is.na(finalmanufgoods_early), (finalmanufgoods_early * p) + intermediatemanufgoods_early * (1-p), (finalmanufgoods_full * p) + intermediatemanufgoods_full * (1-p)))

####
## IMPORT PRICES
####

##UVI20000 - Imports
##UVI34000 - Imports Excl Crude
##UVI50000 - All Items

codes <- c("UVI20000", "UVI34000", "UVI50000")

m <- tibble(time_period = character())

for (code in codes){

    temp_m <- readxl::read_excel(here::here("data", "xm20102022.xlsx")) %>%
                    filter(H03 == code) %>%
                    rename(type = H03) %>%
                    select(-c("H01", "H02", "H04", "H05", "H17", "H18", "H25")) %>%
                    pivot_longer(-type, names_to = "period", values_to = code) %>%
                    mutate(time_period = paste0(stringr::str_sub(period, 5, 8), "-", stringr::str_sub(period, 3, 4), "-01")) %>%
                    select(time_period, !!sym(code)) %>%
                    mutate(!!sym(code) := as.numeric(!!sym(code))) %>%
                    arrange(time_period)

    m <- full_join(m, temp_m, by = "time_period")
}


m <- m %>%
    mutate(uvi2  = log(UVI20000) - log(lag(UVI20000, n = 1)), 
            uvi34  = log(UVI34000) - log(lag(UVI34000, n = 1)),
            uvi5 = log(UVI50000) - log(lag(UVI50000, n = 1)), 
            time_period = as.Date(time_period)) %>%
    select(-c("UVI20000", "UVI34000", "UVI50000")) 

####
## CPI
####

## Headline

cpi <- read_dataset(id = "CPI_ANL_SERIES", 
                        series_key = "CPS00000") %>%
                        as_tibble() %>%
                        rename(value = "CPS00000") %>%
                        filter(time_period >= as.Date("2008-01-01") & time_period <= as.Date("2026-03-01")) %>%
                        mutate(cpi = log(value) - log(lag(value, n = 1)))

####
## Output Gap Indicators
####

##BCI BER

bci <- readxl::read_excel(here::here("data", "BCI DATA", "BCI_April_2026.xls")) %>%
    rename(time_period = "...1", 
            leading = "Leading\n indicator", 
            lagging = "Lagging\n indicator",
            coincident = "Coincident\n indicator") %>%
    filter(!is.na(time_period)) %>%
    mutate(time_period = lubridate::floor_date(lubridate::as_date(time_period), "month"),
    leading_lagged = lag(leading, n = 1), 
    coincident_lagged = lag(coincident, n = 1),
    across(c(leading, lagging, coincident, leading_lagged, coincident_lagged), as.numeric), 
    leading_detrended = leading - leading_lagged,
    coincident_detrended = coincident - coincident_lagged) %>%
    filter(time_period >= lubridate::ymd("2008-01-01") & time_period <= lubridate::ymd("2026-01-28")) %>%
    group_by(time_period)


hp <-  mFilter::hpfilter(bci$coincident, freq = 14400, type = "lambda")

bci <- bci %>%
    ungroup() %>%
    mutate(outputgap_bci = hp$cycle)

bci_ts <- ts(bci %>% select(!time_period), start = c(2008, 1), frequency = 12)

bci_aligned <- window(bci_ts[, "coincident_detrended"],
                      start = c(2008, 1),
                      end   = c(2026, 1))

##SARB 

sarb_gdp <- read_dataset(id = "QB_NATLACC", 
                        series_key = "KBP6006D.Q.R.S.LA") %>%
                        as_tibble() %>%
                        rename(gdp = "KBP6006D.Q.R.S.LA") %>%
                        mutate(lgdp = log(gdp)) %>%
                        filter(time_period >= as.Date("2008-01-01") & time_period <= as.Date("2026-02-01")) %>%
                        mutate(detrended_gdp = gdp - lag(gdp)) 

sarb_gdp_ts <- ts(sarb_gdp %>% select(lgdp), 
    start = c(2008, 1), 
    frequency = 4)

model_dc <- tempdisagg::td(sarb_gdp_ts ~ 1, 
    to = 12,
     method = "denton-cholette")

gdp_monthly_dc <- predict(model_dc)

gdp_monthly_dc <- data.frame(
  time_period = zoo::as.Date(zoo::as.yearmon(time(gdp_monthly_dc))),
  dc_monthly = as.numeric(gdp_monthly_dc)
)

hp_dc <-  mFilter::hpfilter(gdp_monthly_dc$dc_monthly, freq = 14400, type = "lambda")

gdp_monthly_dc <- gdp_monthly_dc %>%
  mutate(outputgap_dc = hp_dc$cycle)

model_cl <- tempdisagg::td(sarb_gdp_ts ~ bci_aligned,
    to = 12,
    method = "chow-lin-maxlog")

gdp_monthly_cl <- predict(model_cl)

gdp_monthly_cl <- data.frame(
  time_period = zoo::as.Date(zoo::as.yearmon(time(gdp_monthly_cl))),
  cl_monthly = as.numeric(gdp_monthly_cl)
) 

hp_cl <-  mFilter::hpfilter(gdp_monthly_cl$cl_monthly, freq = 14400, type = "lambda")

gdp_monthly_cl <- gdp_monthly_cl %>%
  mutate(outputgap_cl = hp_cl$cycle)

outputgap <- left_join(bci, left_join(gdp_monthly_cl, gdp_monthly_dc, by = "time_period"), by = "time_period") %>%
    select(time_period, outputgap_bci, outputgap_dc, outputgap_cl)


####
## Exchange Rate Data
####

##USD/ZAR from Fred
usdzar <- fredr::fredr(series_id = "DEXSFUS", 
                    observation_start = as.Date("2008-01-01"), 
                    observation_end = as.Date("2026-03-01"),   
                    frequency = "m") %>%
                    mutate(time_period = as.Date(paste0(stringr::str_sub(as.character(date), 1, 7), "-01"))) %>%
                    select(time_period, value) %>%
                    rename(rate = value) %>%
                    mutate(usdzar_fred = log(rate) - log(lag(rate, n = 1))) 

View(usdzar)

##How many rands for a dollar, positive value means rand has depreciated

##NEER from SARB
neer <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
    mutate(time_period = lubridate::my(Date),
    neer_sarb = log(Value) - log(lag(Value, n = 1))) %>%
    arrange(time_period) %>%
    select(-Date) %>%
    filter(time_period >= as.Date("2008-01-01") & time_period <= as.Date("2026-03-01"))

####
## Brent Crude  Data
####

oil <- fredr::fredr(series_id = "POILBREUSDM", 
                    observation_start = as.Date("2008-01-01"), 
                    observation_end = as.Date("2026-03-01"),   
                    frequency = "m") %>%
                    mutate(time_period = as.Date(paste0(stringr::str_sub(as.character(date), 1, 7), "-01"))) %>%
                    select(time_period, value) %>%
                    mutate(oil = log(value) - log(lag(value, n = 1))) %>%
                    select(-value)


shock <- readxl::read_excel(here::here("data", "oilsupplynews", "oilSupplyNewsShocks_2025M06.xlsx"), sheet = "Monthly") %>%
    select(Date, `Oil supply news shock`) %>%
    mutate(time_period = as.Date(paste0(stringr::str_sub(Date, 1, 4), "-", stringr::str_sub(Date, 6, 7), "-01"), format = "%Y-%m-%d"), oilshock = `Oil supply news shock`) %>%
    select(time_period, oilshock) 

oil <- left_join(oil, shock, by = "time_period")

####

##MERGE DATA

####

new_sample <- m %>%
  full_join(ppi,        by = "time_period") %>%
  full_join(cpi,        by = "time_period") %>%
  full_join(outputgap, by = "time_period") %>%
  full_join(neer,       by = "time_period") %>%
  full_join(usdzar,     by = "time_period") %>%
  full_join(oil,        by = "time_period") %>%
  filter(time_period >= as.Date("2008-02-01") & time_period <= as.Date("2025-12-01"))

filename <- file.path(here::here(), "data", "samples", "newsample.csv")
write.csv(new_sample, filename)



new_sample <- readr::read_csv(here::here("data", "samples", "newsample.csv")) %>%
    mutate(outputgap_bci = as.numeric(scale(outputgap_bci)))

historical_sample <- readr::read_csv(here::here("data", "samples", "historicalsample.csv")) %>%
    mutate(outputgap_bci = as.numeric(scale(outputgap_bci)))



neer_full <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
    mutate(time_period = lubridate::my(Date)) %>%
    arrange(time_period) %>%
    mutate(neer_sarb = -(log(Value) - log(lag(Value, n = 1)))) %>%
    arrange(time_period) %>%
    select(-Date) %>%
    filter(time_period >= as.Date("1992-01-01") & time_period <= as.Date("2026-03-01"))
View(neer_full)

full_sample <- bind_rows(new_sample %>% filter(time_period > "2012-12-01"), historical_sample) %>%
    select(-neer_sarb) %>%
    left_join(neer_full %>% select(-Value), by = "time_period")



filename <- file.path(here::here(), "data", "samples", "fullsample.csv")
write.csv(full_sample, filename)
