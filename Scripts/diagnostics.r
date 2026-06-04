##############################################################################################################################################################################

############################################################################## Run Diagnostics ##############################################################################

##############################################################################################################################################################################


    pacman::p_load(dplyr, tidyr, econdatar, ggplot2, showtext, tseries, urca, kableExtra)
library(showtext)
library(sysfonts)


#### 
## Graph Theme
####


# Load a serif font (matches academic typesetting)
font_add_google("Source Serif 4", "source_serif")
font_add_google("Source Sans 3", "source_sans")
showtext_auto()

# ── Palette ────────────────────────────────────────────────────────────────────
# Muted, print-safe colours that distinguish well in greyscale too
erpt_palette <- c(
    "oilshock"      = "#E69F00",   # amber
    "outputgap_bci" = "#56B4E9",   # sky blue
    "outputgap_dc"  = "#56B4E9",   # sky blue (same concept)
    "usdzar_fred"   = "#000000",   # black
    "neer_sarb"     = "#000000",   # black
    "m"             = "#009E73",   # teal green
    "ppi"           = "#0072B2",   # deep blue
    "ppi_manuf"     = "#0072B2",   # deep blue
    "cpi"           = "#D55E00"    # vermillion
)


# ── Theme ──────────────────────────────────────────────────────────────────────
theme_publication <- function(base_size = 11, base_family = "source_serif") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Panel
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      panel.grid.major  = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.minor  = element_blank(),

      # Axes
      axis.line         = element_blank(),           # border handles this
      axis.ticks        = element_line(colour = "grey20", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.text         = element_text(size = rel(0.85), colour = "grey20",
                                       family = "source_sans"),
      axis.title        = element_text(size = rel(0.90), colour = "grey10",
                                       family = "source_sans",
                                       margin = margin(t = 4, r = 4)),

      # Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      legend.key.width  = unit(18, "pt"),
      legend.key.height = unit(9, "pt"),
      legend.text       = element_text(size = rel(0.80), family = "source_sans",
                                       colour = "grey20"),
      legend.title      = element_text(size = rel(0.85), family = "source_sans",
                                       face = "bold", colour = "grey10"),
      legend.position   = "bottom",
      legend.margin     = margin(t = 4),
      legend.spacing.x  = unit(6, "pt"),

      # Strip (for facets)
      strip.background  = element_rect(fill = "grey93", colour = "grey20",
                                       linewidth = 0.4),
      strip.text        = element_text(size = rel(0.85), family = "source_sans",
                                       face = "bold", colour = "grey10",
                                       margin = margin(3, 6, 3, 6)),

      # Titles
      plot.title        = element_text(size = rel(1.5), face = "bold",
                                       family = "source_serif", colour = "grey10",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = rel(1), family = "source_sans",
                                       colour = "grey30",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = rel(1), family = "source_sans",
                                       colour = "grey40", hjust = 0,
                                       margin = margin(t = 6)),
      plot.margin       = margin(12, 14, 10, 12),
      plot.background   = element_rect(fill = "white", colour = NA),

      complete = TRUE
    )
}



# full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
#       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
#     filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1990-02-01")) 

# full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

# short <- full_filtered %>% filter(time_period < as.Date("2013-01-01"))


# ####
# ## Show the 3 measures of output gap to support one another



# output_long <- outputgap %>%
#   pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
#   group_by(type) %>%
#   mutate(value_n = scale(value))


# ##NEER from SARB
# neer <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
#   mutate(time_period = lubridate::my(Date),
#   neer_sarb = log(Value) - log(lag(Value, n = 1))) %>%
#   select(-Date) %>%
#   filter(time_period >= as.Date("1992-01-01") & time_period <= as.Date("2026-03-01"))
# View(neer)
# full_corr <- readr::read_csv(here::here("data", "samples", "fullsample.csv"))  %>%
#   filter(time_period >= as.Date(2013)) %>%
# select(outputgap_bci, outputgap_cl, outputgap_dc) %>%
# rename("SARB BCI" = "outputgap_bci",
#       "Denton-Cholette" = "outputgap_dc",
#       "Chow-Lin" = "outputgap_cl"
# ) %>%
# cor(use = "complete.obs")
# print(full_corr)

# full_corr_diag <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
# select(time_period, outputgap_bci, outputgap_cl, outputgap_dc) %>%
# filter(time_period <= as.Date("2022-12-01")) %>%
# mutate(period = if_else(time_period <= as.Date("2010-01-01"), 
#                         "historical", "new")) %>%
# group_by(period) %>%
# summarise(
#   mean_bci = mean(outputgap_bci, na.rm = TRUE) / 100,
#   mean_cl  = mean(outputgap_cl,  na.rm = TRUE),
#   mean_dc  = mean(outputgap_dc,  na.rm = TRUE),
#   sd_bci   = sd(outputgap_bci,   na.rm = TRUE) / 100,
#   sd_cl    = sd(outputgap_cl,    na.rm = TRUE),
#   sd_dc    = sd(outputgap_dc,    na.rm = TRUE),
#   cor_bci_dc = cor(outputgap_bci, outputgap_dc, use = "complete.obs")
# )
  
# full_corr_diag <- full_corr_diag %>%
# mutate(
#   Period = tools::toTitleCase(period),
#   `SARB BCI` = paste0(
#     formatC(mean_bci, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_bci, format = "f", digits = 4),
#     ")"
#   ),
#   `Chow-Lin` = paste0(
#     formatC(mean_cl, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_cl, format = "f", digits = 4),
#     ")"
#   ),
#   `Denton-Cholette` = paste0(
#     formatC(mean_dc, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_dc, format = "f", digits = 4),
#     ")"
#   ),
#   `BCI $\\times$ DC` = formatC(cor_bci_dc, format = "f", digits = 3)
# ) %>%
# select(Period, `SARB BCI`, `Chow-Lin`, `Denton-Cholette`, `BCI $\\times$ DC`) %>%
# kbl(
#   booktabs = TRUE,
#   escape   = FALSE,
#   align    = c("l", "c", "c", "c", "c"),
#   caption  = "Output Gap Proxy Comparison by Subsample"
# ) %>%
# kable_styling(
#   latex_options     = c("hold_position"),
#   bootstrap_options = c("striped", "hover"),  # HTML only
#   full_width        = FALSE,
#   position          = "center"
# ) %>%
# add_header_above(
#   c(" " = 1, "Mean (SD)" = 3, " " = 1),
#   bold   = TRUE,
#   line   = TRUE,
#   escape = FALSE
# ) %>%
# row_spec(0, bold = TRUE) %>%
# footnote(
#   general         = "HP-filtered output gap proxies ($\\lambda = 14{,}400$, monthly). Standard deviations in parentheses. Sample split at 2008-10. BCI $\\times$ CL denotes the Pearson correlation between the SARB BCI and Chow-Lin measures within each subsample.",
#   general_title   = "\\textit{Notes:}",
#   escape          = FALSE,
#   threeparttable  = TRUE
# )

# print(full_corr_diag)




# sarb_gdp <- read_dataset(id = "QB_NATLACC", 
#                       series_key = "KBP6006D.Q.R.S.LA") %>%
#                       as_tibble() %>%
#                       rename(gdp = "KBP6006D.Q.R.S.LA") %>%
#                       mutate(lgdp = log(gdp),
#           detrended_gdp = gdp - lag(gdp)) %>%
#   filter(time_period >= as.Date("1992-01-01") & 
#           time_period <= as.Date("2026-02-01")) %>%
#           mutate(gdp_cycle = mFilter::hpfilter(lgdp, freq = 1600)$cycle) 
          
            

# bci <- readxl::read_excel(here::here("data", "BCI DATA", "BCI_April_2026.xls")) %>%
#   rename(time_period = "...1", 
#           leading = "Leading\n indicator", 
#           lagging = "Lagging\n indicator",
#           coincident = "Coincident\n indicator") %>%
#   filter(!is.na(time_period)) %>%
#   mutate(time_period = lubridate::floor_date(lubridate::as_date(time_period), "month"),
#   leading_lagged = lag(leading, n = 1), 
#   coincident_lagged = lag(coincident, n = 1),
#   across(c(leading, lagging, coincident, leading_lagged, coincident_lagged), as.numeric), 
#   leading_detrended = leading - leading_lagged,
#   coincident_detrended = coincident - coincident_lagged) %>%
#   filter(time_period >= lubridate::ymd("1992-01-01") & time_period <= lubridate::ymd("2026-01-28")) %>%
#   group_by(time_period)

# sarb_gdp <- full_join(sarb_gdp, bci %>% select(time_period, coincident), by = "time_period")

# gdp_ts <- ts(sarb_gdp$lgdp, 
#             start = c(1992, 1), 
#             frequency = 4)
# bci_ts <- ts(sarb_gdp$coincident_detrended, 
#             start = c(1992, 1), 
#             frequency = 12)

# fit <- tempdisagg::td(gdp_ts ~ bci_ts, method = "chow-lin-maxlog")
# summary(fit)

# View(sarb_gdp)
# model_cl <- tempdisagg::td(sarb_gdp_ts ~ 1, 
#   to = 12,
#     method = "denton-cholette")


# ####

# ## Plot All Variables for each Sample

# shock_cor <- full_filtered %>%
#   mutate(shock_lag = lag(full$oilshock)) %>%
#   select(oilshock, shock_lag) %>%
#   cor(use = "complete.obs")
# print(full)
# ####

# samples <- c("new", "historical", "full")

# df_wide <- readr::read_csv(here::here("data", "samples", "newsample.csv")) %>%
#   select(-c("...1", "uvi2", "uvi34", "uvi5"))


##############################################################################################################################################################################



################################################################################ BEGIN HERE ################################################################################



##############################################################################################################################################################################







full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1990-02-01")) 


full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))


low_inflation <- full_filtered %>% filter(time_period >= as.Date("2010-01-01"))


early <- full_filtered %>% filter(time_period < as.Date("2010-01-01"))




########
## Check for uncorrelated shocks
#########


df_var <- full_filtered %>%
    select(oilshock, outputgap_dc, usdzar_fred, m, ppi, cpi) %>%
    as.data.frame()
n_lags <- 12

var_model <- do.call(VAR, list(y = df_var, p = as.integer(n_lags), type = "const"))
resids <- cbind(residuals(var_model), oilshock = tail(full$oilshock, nrow(residuals(var_model))))

residcor <- cor(resids) %>%
        as.data.frame() 
View(residcor)

openxlsx::write.xlsx(residcor, 
           file      = here::here("Tables", "residcor.xlsx"),
           sheetName = "Residuals",
           rowNames  = FALSE)

      
View(full)

print(lags)        



##############################################################################################################################################################################

################################################################################ AIC ################################################################################

##############################################################################################################################################################################




aic_var_df <- tibble(Lag = as.numeric(), 
                    AIC = as.numeric(), 
                    BIC = as.numeric())

View(full_filtered)

full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2022-012-01"), time_period >= as.Date("1990-02-01")) 

full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

df_var <- full_filtered %>%
    select(oilshock, outputgap_dc, neer_sarb, m, ppi, cpi) %>%
    as.data.frame() 

lags <- c(1:24)
for (lag in lags){

    oil_exog <- sapply(0:lag, function(l) dplyr::lag(df_var$oilshock, l))
    colnames(oil_exog) <- paste0("oil.l", 0:lag)
    
    var_model <- do.call(VAR, list(y = df_var %>% dplyr::select(-oilshock), p = as.integer(lag), type = "const", exogen = df_var %>% dplyr::select(oilshock) %>% as.matrix()))

    AIC <- as.numeric(AIC(var_model))
    Lag <- as.numeric(lag)
    BIC <- as.numeric(BIC(var_model))

    temp <- tibble(Lag = Lag, AIC = AIC, BIC = BIC)

    aic_var_df <- rbind(aic_var_df, temp)
}

print(aic_var_df)
View(aic_var_df)
View(residcor)

info <- VARselect(
    df_var %>% dplyr::select(-oilshock), 
    lag.max = 24, 
    type = "const",
    exogen = df_var %>% dplyr::select(oilshock) %>% as.matrix()
)

aic_var_df <- tibble(
    Lag = 1:24,
    AIC = info$criteria["AIC(n)", ]
)



cor(full$m_hist, full$uvi34, use = "complete.obs")





full <- full %>%
    arrange(time_period) %>%
    filter(time_period <= as.Date("2022-12-01"))


##############################################################################################################################################################################

################################################################################ Stationarity ################################################################################

##############################################################################################################################################################################


vars <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")
export_adf_results <- function(data, vars, lags = 12,
                                outfile = here::here("main", "adf_results.txt")) {
  library(urca)
  library(knitr)
  library(kableExtra)
  
  specs     <- c("trend", "drift", "none")
  tau_names <- c(trend = "tau3", drift = "tau2", none = "tau1")
  spec_labels <- c(trend = "Trend", drift = "Drift", none = "None")
  
  results <- lapply(vars, function(var) {
    lapply(specs, function(spec) {
      res  <- urca::ur.df(data[[var]], type = spec, lags = lags, selectlags = "AIC")
      s    <- summary(res)
      tau  <- tau_names[spec]
      stat <- s@teststat[1, tau]
      cv   <- s@cval[tau, ]
      data.frame(
        Variable   = case_when(var == "oilshock" ~ "Oil Shock",
                                var == "outputgap_dc" ~ "Demand",
                                var == "neer_sarb" ~ "Exchange Rate",
                                var == "m" ~ "Importer Price Index",
                                var == "ppi" ~ "Producer Price Index",),
        Spec       = as.character(spec_labels[spec]),
        Statistic  = round(stat, 3),
        `1%`       = cv["1pct"],
        `5%`       = cv["5pct"],
        `10%`      = cv["10pct"],
        Reject     = ifelse(stat < cv["5pct"], "Yes", "No"),
        check.names = FALSE
      )
    }) %>% bind_rows()
  }) %>% bind_rows()

  tbl <- results %>%
    select(-Variable) %>%
    kable(
        format      = "latex",
        booktabs    = TRUE,
        linesep     = "",
        caption     = "Augmented Dickey-Fuller Unit Root Tests",
        label       = "tab:adf",
        col.names   = c("Specification", "Statistic", "1\\%", "5\\%", "10\\%", "Reject"),
        escape      = FALSE,
        row.names = FALSE,
        table.envir = "table"
    ) %>%
    pack_rows("Oil Shock",             1, 3)  %>%
    pack_rows("Demand",                4, 6)  %>%
    pack_rows("Exchange Rate",         7, 9)  %>%
    pack_rows("Import Price Index",   10, 12) %>%
    pack_rows("Producer Price Index", 13, 15) %>%
    pack_rows("Consumer Price Index", 16, 18) %>%
    row_spec(3,  hline_after = TRUE) %>%
    row_spec(6,  hline_after = TRUE) %>%
    row_spec(9,  hline_after = TRUE) %>%
    row_spec(12, hline_after = TRUE) %>%
    row_spec(15, hline_after = TRUE) %>%
    row_spec(18, hline_after = TRUE) %>%
    kable_styling(
        latex_options = c("HOLD_position"),
        font_size     = 8,
        full_width    = FALSE
    ) 
  tbl_text <- as.character(tbl)
  writeLines(tbl_text, outfile)
  message("Saved: ", outfile)
}

export_adf_results(
  data = full_filtered,
  vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi"),
  lags = 12,
  outfile = here::here("main", "adf_results.txt")
)

View(full)



samples  <- c("full_filtered", "early", "low_inflation")
sample_names <- c("1990 - 2023", "1990 - 2010", "2010 - 2023")
stationarity <- list()


for (i in 1:length(samples)){
        sample <- samples[i]
        name <- sample_names[i]
        df <- get(sample)
        path <- here::here("main")
        Sample <- tools::toTitleCase(sample)
  
    df <- df %>%
        select(time_period, oilshock, outputgap_dc, neer_sarb, m, ppi, cpi) %>%
        mutate( 
                oilshock = oilshock / 100) %>%
        pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
        mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) 

    plot <- ggplot(data = df %>% filter(type %in% c("oilshock", "outputgap_dc", "neer_sarb", "m" ,"ppi", "cpi"), time_period <= as.Date("2025-06-01")), aes(x = time_period, y = value, colour = type)) +
    geom_line(linewidth = 0.6, alpha = 0.75) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_colour_manual(values = erpt_palette, labels = c("oilshock" = "Oil Shock ", "outputgap_dc" = "Demand", "neer_sarb" = "Exchange Rate", "m" = "Importer Price Index","ppi" = "Producer Price Index", "cpi" = "Consumer Price Index")) +
    scale_y_continuous(expand = expansion(mult = 0.03)) +
    labs(
        x        = NULL,
        y        = NULL,
        colour   = NULL,
        title    = name
    ) +
    theme(
      text         = element_text(family = "serif"),
    plot.title   = element_text(family = "serif"),
    axis.text    = element_text(family = "sans"),
    legend.text  = element_text(family = "sans")
    ) +
    theme_publication(base_size = 11, base_family = "source_serif")
    
  ggsave(filename = paste0("stationarity_", sample,".png"), 
         plot     = plot, 
         path     = path,
         width    = 6.5, 
         height   = 4, 
         dpi      = 225)

         stationarity[[sample]] <- plot
}


combined_stationarity <- wrap_plots(stationarity, nrow = 3, ncol = 1) +
  plot_layout(guides = "collect", 
      axes = "collect", 
      heights = c(1, 1, 1),   # equal row heights — reduce total by adjusting ggsave height
       widths  = c(1, 1) ) +
  plot_annotation(
    theme = theme(
      plot.caption  = element_text(hjust = 0, size = 9, family = "source_sans"),
      legend.position = "bottom",
      legend.text  = element_text(family = "source_sans", size = rel(2.5)),
      legend.title  = element_text(family = "source_sans", ),
      text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_sans", size = rel(2.5)),
    legend.key.size = unit(2.5, "cm"),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    )
  ) &
  theme(
    text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_sans", size = rel(2.5)),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    legend.text  = element_text(family = "source_sans", size = rel(2.5)),
    legend.key.size = unit(2.5, "cm"),
    legend.title = element_text(family = "source_sans"),
    strip.text   = element_text(family = "source_sans"),
    plot.caption = element_text(family = "source_sans"),
    plot.margin  = margin(2, 6, 2, 4)
  )

ggsave(
    filename = "stationarity_combined.png",
    plot     = combined_stationarity,
    path     = here::here("main"),
    width    = 10,
    height   = 10,
    dpi      = 300,
    device   = "png"
)



descriptive_table <- function(data, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")) {
  
  p1 <- data %>% filter(time_period >= as.Date("1990-02-01") & time_period <= as.Date("2008-12-01"))
  p2 <- data %>% filter(time_period >= as.Date("2009-01-01") & time_period <= as.Date("2022-12-01"))

  
  fmt <- function(x) paste0(round(mean(x, na.rm = TRUE), 3), " (", round(sd(x, na.rm = TRUE), 3), ")")
  
  rows <- lapply(vars, function(var) {
    tibble(
      Variable    = var,
      Full_Sample = fmt(data[[var]]),
      `1990-2008` = fmt(p1[[var]]),
      `2009-2023` = fmt(p2[[var]])
    )
  })
  
  bind_rows(rows)
}

desc <- descriptive_table(data = full_filtered)
View(desc)
writexl::write_xlsx(desc, path = here::here("Tables", "descriptives.xlsx"))


##############################################################################################################################################################################

################################################################################ Normality ###################################################################################

##############################################################################################################################################################################

normality_test <- function(data, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")) {
    table <- tibble()
    for (var in vars){
        jb <- tseries::jarque.bera.test(data[[var]])

        if (as.numeric(jb$p.value) < 0.001){
            jbp <- "p < 0.01"
        } else{
            jbp <- round(jb$p.value, 3)
        }
        row <- tibble(
        name = var,
        skewness = round(moments::skewness(data[[var]]), 3),
        kurtosis = round(moments::kurtosis(data[[var]]), 3),
        jbstat = round(jb$statistic, 3),
        jbp = jbp)
    table <- bind_rows(table, row)
    }
    return(table)


}

norm <- normality_test(data = full_filtered)


writexl::write_xlsx(norm, path = here::here("Tables", "normality.xlsx"))




##############################################################################################################################################################################

########################################################################## Import Indices ####################################################################################

##############################################################################################################################################################################

overlap <- full %>%
    filter(time_period >= as.Date("2010-01-01"), 
            time_period <= as.Date("2012-12-01")) %>%
    arrange(desc(time_period))

ccf <- broom::tidy(ccf(overlap$uvi34, overlap$m_hist, na.action = na.omit, lag.max = 6)) %>%
        knitr::kable(format = "latex", digits = 4, booktabs = TRUE, col.names = c("k Lag", "Correlation"),
        caption = "Correlation between UVI(t+k) and SIC(t)")
print(ccf)             
writeLines(ccf, here::here("tables", "uvi_sic_ccf.tex"))

print(ccf(overlap$uvi34, overlap$m_hist, na.action = na.omit, lag.max = 6))
t_test <- broom::tidy(t.test(lag(overlap$uvi34, n = 1), overlap$m_hist, paired = TRUE)) %>%
          select(estimate, statistic, p.value, conf.low, conf.high, parameter) %>%
          rename("Mean diff." = estimate, "t" = statistic, "p" = p.value,
                "CI low" = conf.low, "CI high" = conf.high, "df" = parameter) %>%
          knitr::kable(format = "latex", digits = 4, booktabs = TRUE,
                caption = "Paired t-test: UVI vs SIC import price series (lagged)")

print(t_test)
writeLines(t_test, here::here("tables", "uvi_sic_ttest.tex"))



# VAR consumes first p=9 rows for lags; residuals start at row 13
# break_idx is the position within the residual sequence

chow_df <- full_sample %>%
    filter(time_period <= as.Date("2022-12-01"), time_period > as.Date("1990-01-01")) %>%
    select(time_period, oilshock, outputgap_dc, neer_sarb, m, ppi, cpi) %>%
    mutate(d_step = as.integer(time_period >= as.Date("2010-02-01")))


break_idx <- which(chow_df$time_period == as.Date("2010-03-01")) - 9
print(break_idx)

oil_exog <- sapply(0:9, function(l) lag(chow_df$oilshock, l)) %>%
    `colnames<-`(paste0("oil.l", 0:9))


var_model_chow <- VAR(chow_df %>% select(-oilshock, -time_period, -d_step), 
                 p = 9, 
                 type = "const",
                 exogen = cbind(oil_exog, d_step = chow_df$d_step))


cat("Break index in residual sequence:", break_idx, "\n") 
chow_manual <- function(eq, break_idx) {
    mm  <- model.matrix(eq)
    y   <- model.response(model.frame(eq))
    k   <- ncol(mm)
    n   <- nrow(mm)
    
    ssr_full  <- sum(resid(eq)^2)
    ssr_pre   <- sum(lm.fit(mm[1:break_idx, ],          y[1:break_idx])$residuals^2)
    ssr_post  <- sum(lm.fit(mm[(break_idx+1):n, ], y[(break_idx+1):n])$residuals^2)
    
    F_stat <- ((ssr_full - ssr_pre - ssr_post) / k) / 
              ((ssr_pre + ssr_post) / (n - 2 * k))
    p_val  <- pf(F_stat, df1 = k, df2 = n - 2*k, lower.tail = FALSE)
    
    c(F = round(F_stat, 3), p = round(p_val, 4))
}

for (eq_name in names(var_model_chow$varresult)) {
    res <- chow_manual(var_model_chow$varresult[[eq_name]], break_idx)
    cat(eq_name, ": F =", res["F"], ", p =", res["p"], "\n")
}

chow_results <- lapply(names(var_model_chow$varresult), function(eq_name) {
    res <- chow_manual(var_model_chow$varresult[[eq_name]], break_idx)
    data.frame(equation = eq_name, F_stat = res["F"], p_value = res["p"])
}) %>% bind_rows()

write.csv(chow_results, here::here("tables", "chow_test_2010.csv"), row.names = FALSE)



imports <- full_filtered %>%
    select(time_period, m_hist, uvi34_l) %>%
    pivot_longer(-time_period, names_to = "series", values_to = "value") %>%
    mutate(series = recode(series, "m_hist" = "SIC", "uvi34_l" = "UVI")) %>%
    ggplot(aes(x = time_period, y = value, colour = series)) +
    geom_line() +
    coord_cartesian(xlim = c(as.Date("2009-01-01"), as.Date("2014-01-01"))) +
    theme_publication(base_size = 11, base_family = "source_serif")

ggsave(filename = "import_indices.png", path = here::here("descriptives"))


####
### Arithmetic vs Geometric
####


folders <- c("xm20102022.xlsx", "uvi16_25.xlsx")
codes <- c("UVI20000", "UVI34000", "UVI50000")

uvi <- tibble(time_period = character())
for (folder in folders){
    m_temp <- tibble(time_period = character())

    for (code in codes){
        path <- paste0(here::here(), "/data")
        temp_m <- readxl::read_excel(paste0(path, "/", folder)) %>%
                        filter(H03 == code) %>%
                        rename(type = H03) %>%
                        select(-c("H01", "H02", "H04", "H05", "H17", "H18", "H25")) %>%
                        pivot_longer(-type, names_to = "period", values_to = code) %>%
                        mutate(time_period = paste0(stringr::str_sub(period, 5, 8), "-", stringr::str_sub(period, 3, 4), "-01")) %>%
                        select(time_period, !!sym(code)) %>%
                        mutate(!!sym(code) := as.numeric(!!sym(code))) %>%
                        arrange(time_period) 
        m_temp <- full_join(m_temp, temp_m, by = "time_period")

        }

        m_temp <- m_temp %>%
                mutate(uvi2  = log(UVI20000) - log(lag(UVI20000, n = 1)), 
                        uvi34  = log(UVI34000) - log(lag(UVI34000, n = 1)),
                        uvi5 = log(UVI50000) - log(lag(UVI50000, n = 1)))
        if (folder == "xm20102022.xlsx"){
            temp_m <- temp_m %>%
                    filter(time_period <= as.Date("2016-01-01"))
    }

    uvi <- full_join(m, m_temp, by = "time_period", suffix = c(".arit", ".geom"))
}


overlap_uvi <- m %>%
    filter(time_period >= as.Date("2016-02-01"), 
            time_period <= as.Date("2022-12-01")) %>%
    arrange(desc(time_period))

cor_uvi <- cor(overlap_uvi$uvi34.arit, overlap_uvi$uvi34.geom, use = "complete.obs")
print(cor_uvi)  # [1] 0.7657967

t_test_uvi <- broom::tidy(t.test(overlap_uvi$uvi34.arit, overlap_uvi$uvi34.geom, paired = TRUE)) %>%
          select(estimate, statistic, p.value, conf.low, conf.high, parameter) %>%
          rename("Mean diff." = estimate, "t" = statistic, "p" = p.value,
                "CI low" = conf.low, "CI high" = conf.high, "df" = parameter) %>%
          knitr::kable(format = "latex", digits = 4, booktabs = TRUE,
                caption = "Paired t-test: Arithmetic vs Geometric UVI ")


print(t.test(overlap_uvi$uvi34.arit, overlap_uvi$uvi34.geom, paired = TRUE))
print(t_test_uvi)
writeLines(t_test_uvi, here::here("tables", "arit_geom_ttest.tex"))
