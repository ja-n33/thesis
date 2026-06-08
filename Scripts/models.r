##############################################################################################################################################################################

############################################################################## Run VAR Models ##############################################################################

##############################################################################################################################################################################

pacman::p_load(dplyr, tidyr, econdatar, ggplot2, vars, conflicted, lpirfs, patchwork, magick, cowplot, knitr, kableExtra)


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# ── Theme ──────────────────────────────────────────────────────────────────────

sysfonts::font_add_google("Source Serif 4", "source_serif")
sysfonts::font_add_google("Source Sans 3",  "source_sans")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 150)


extrafont::loadfonts(device = "all")

sysfonts::font_add_google("Source Sans 3", "source_sans")



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
      axis.text         = element_text(size = rel(1.2), colour = "grey20",
                                       family = "source_sans"),
      axis.title        = element_text(size = rel(1.4), colour = "grey10",
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
      plot.title        = element_text(size = rel(1.3), face = "bold",
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

##############################################################################################################################################################################

########################################################################      Collect Samples            ####################################################################

##############################################################################################################################################################################


here::here()


full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
      select(-oil) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d"),
              oil = oilshock) %>%
      filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1988-01-01")) 


full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

full_m <-  full %>% filter(time_period < as.Date("2023-01-01"))

low_inflation <- full_filtered %>% filter(time_period >= as.Date("2010-01-01"))
low_inflation_m <- low_inflation


early <- full_filtered %>% filter(time_period < as.Date("2010-01-01"))
early_m <- early

for (var in c("m", "ppi", "cpi", "neer_sarb")) {
    ts_x <- ts(full_filtered[[var]], start = c(1990, 2), frequency = 12)
    png(here::here("descriptives", paste0("monthplot_", var, ".png")),
        width = 1200, height = 600, res = 150)
    monthplot(ts_x, main = var)
    dev.off()
}
View(full)
##############################################################################################################################################################################

######################################################################### Vector Autoregression Models ####################################################################

##############################################################################################################################################################################

  
plot_var <- function(sample = full, 
                var_list = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi"),
                n_lags = 9, 
                name = "Full Sample", 
                imp      = "neer_sarb",
                resp = c("ppi_full", "cpi"), 
                horizon = 15, 
                break_date = NA){

                coefficients_m <- NULL
                hor <- as.numeric(horizon)
                name <- as.character(name)

                if (imp == "usdzar_fred"){
                  measure <- "Currency Measure is the USD/ZAR Exchange Rate.\nShaded regions reflect bootstrapped 95% confidence intervals."
                } else {
                  measure <- "Currency Measure is the SARB Nominal Effective Exchange Rate.\nShaded Regions reflect bootstrapped 95% confidence intervals."
                }

                file_name <- paste0(gsub(" ", "", tolower(name)), "_p_", as.character(n_lags), ".png")
                file_name_adj <- paste0(gsub(" ", "", tolower(name)), "_adj_p_", as.character(n_lags), ".png")
                acfpath   <- file.path(here::here(), "acf", file_name)
                irfpath   <- file.path(here::here(), "irf", file_name)
                plot_title <- name

                # Build df_var
                if (!is.na(break_date)){
                  df_var <- sample %>%
                    mutate(d_imp = ifelse(time_period >= break_date, 1L, 0L)) %>%
                    dplyr::select(time_period, all_of(var_list), d_imp)
                } else {
                  df_var <- sample %>%
                    dplyr::select(time_period, all_of(var_list))
                }

                keep <- df_var$time_period >= lubridate::ymd("1990-02-01")


                oil_var <- var_list[1]


                # Build exogenous matrix
                if (oil_var == "oilshock") {
                  oil_exog <- sapply(0:as.numeric(n_lags), function(l) dplyr::lag(df_var$oilshock, l))
                  colnames(oil_exog) <- paste0("oil.l", 0:as.numeric(n_lags))

                  if (!is.na(break_date)) {
                    exog_mat <- cbind(oil_exog, d_imp = df_var$d_imp)
                    exog_mat <- exog_mat[keep, ]
                  } else {
                    exog_mat <- oil_exog
                    exog_mat <- exog_mat[keep, ]
                  }
                } else {
                  if (!is.na(break_date)) {
                    exog_mat <-  as.matrix(df_var$d_imp)
                    exog_mat <- exog_mat[keep, ]
                  } 
                }

                df_var   <- df_var[keep, ] %>% dplyr::select(-time_period) %>% na.omit()

                # Fit VAR
                if (oil_var == "oilshock"){
                  if (!is.na(break_date)){
                    var_model <- do.call(VAR, list(
                      y      = df_var %>% dplyr::select(-oilshock, -d_imp),
                      p      = as.integer(n_lags),
                      type   = "const",
                      exogen = exog_mat))
                  } else {
                    var_model <- do.call(VAR, list(
                      y      = df_var %>% dplyr::select(-oilshock),
                      p      = as.integer(n_lags),
                      type   = "const",
                      exogen = exog_mat))
                  }
                } else {
                  if (!is.na(break_date)){
                    var_model <- do.call(VAR, list(
                      y      = df_var %>% dplyr::select(-d_imp),
                      p      = as.integer(n_lags),
                      type   = "const",
                      exogen = exog_mat))
                  } else {
                    var_model <- do.call(VAR, list(
                      y      = df_var,
                      p      = as.integer(n_lags),
                      type   = "const"))
                  }
                }

                var_summary <- summary(var_model)

                coefficients_cpi <- var_summary$varresult$cpi

                if ("ppi_full" %in% var_list){
                  coefficients_ppi <- var_summary$varresult$ppi_full
                } else {
                  coefficients_ppi <- var_summary$varresult$ppi_all
                }

                if ("uvi34" %in% var_list){
                  coefficients_m <- var_summary$varresult$uvi34
                } else if ("m_all" %in% var_list){
                  coefficients_m <- var_summary$varresult$m_all
                } else if ("m_manuf" %in% var_list){
                  coefficients_m <- var_summary$varresult$m_manuf
                }

                resids <- residuals(var_model)
                ppi_var <- intersect(c("ppi", "ppi_full", "ppi_manuf"), var_list)
                m_var   <- intersect(c("m", "m_manuf", "m_all", "uvi34"), var_list)
                # Residual labelling
                resid_names <- c()
                if ("oilshock" %in% var_list) {
                    resid_names <- c(resid_names, "Demand Shock")
                    if ("int_eff" %in% var_list) resid_names <- c(resid_names, "Interest Rate Shock")
                    resid_names <- c(resid_names, "Exchange Rate Shock")
                } else if ("oil" %in% var_list) {
                    resid_names <- c(resid_names, "Oil Shock", "Demand Shock")
                    if ("int_eff" %in% var_list) resid_names <- c(resid_names, "Interest Rate Shock")
                    resid_names <- c(resid_names, "Exchange Rate Shock")
                }
                if (length(m_var) > 0)        resid_names <- c(resid_names, "Import Price Shock")
                if (length(ppi_var) > 0)      resid_names <- c(resid_names, "PPI Shock")
                resid_names <- c(resid_names, "CPI Shock")

                resids_df <- resids %>%
                    as.data.frame() %>%
                    setNames(resid_names)

                png(filename = acfpath, width = 1200, height = 800, res = 150)
                par(mfrow = c(2, 3))
                for (col in colnames(resids_df)) {
                  acf(resids_df[, col], main = col, lag.max = 24)
                }
                dev.off()

                imp_sd       <- sd(resids[, imp], na.rm = TRUE)
                scale_factor <- 0.01 / imp_sd

                # IRF
                if (oil_var == "oilshock") {
                  irf_results <- do.call(irf, list(
                    x          = var_model,
                    impulse    = imp,
                    response   = c(imp, resp),
                    n.ahead    = hor,
                    boot       = TRUE,
                    ci         = 0.95,
                    cumulative = TRUE,
                    runs       = 500,
                    exogen     = exog_mat
                  ))
                } else {
                  if (!is.na(break_date)){
                  irf_results <- do.call(irf, list(
                    x          = var_model,
                    impulse    = imp,
                    response   = c(imp, resp),
                    n.ahead    = hor,
                    boot       = TRUE,
                    ci         = 0.95,
                    cumulative = TRUE,
                    runs       = 500,
                    exogen = exog_mat
                  ))
                  } 
                  else {
                     irf_results <- do.call(irf, list(
                    x          = var_model,
                    impulse    = imp,
                    response   = c(imp, resp),
                    n.ahead    = hor,
                    boot       = TRUE,
                    ci         = 0.95,
                    cumulative = TRUE,
                    runs       = 500
                  ))
                  }
                }

                # Rescale
                irf_results$irf[[imp]]   <- irf_results$irf[[imp]]   * scale_factor
                irf_results$Lower[[imp]] <- irf_results$Lower[[imp]] * scale_factor
                irf_results$Upper[[imp]] <- irf_results$Upper[[imp]] * scale_factor

                xr_response <- irf_results$irf[[imp]][, imp]

                # Build irf_df

                irf_df <- data.frame(
                    horizon  = 0:hor,
                    estimate = irf_results$irf[[imp]][, "cpi"]   * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                )

                if (length(ppi_var) > 0){
                    irf_df <- bind_rows(
                        data.frame(
                            horizon  = 0:hor,
                            estimate = irf_results$irf[[imp]][, ppi_var]   * 100,
                            lower    = irf_results$Lower[[imp]][, ppi_var] * 100,
                            upper    = irf_results$Upper[[imp]][, ppi_var] * 100,
                            response = "Producer Price Index"
                        ),
                        irf_df
                    )
                }

                if (length(m_var) > 0){
                    irf_df <- bind_rows(irf_df,
                        data.frame(
                            horizon  = 0:hor,
                            estimate = irf_results$irf[[imp]][, m_var]   * 100,
                            lower    = irf_results$Lower[[imp]][, m_var] * 100,
                            upper    = irf_results$Upper[[imp]][, m_var] * 100,
                            response = "Import Price Index"
                        )
                    )
                }
                                

                y_min <- max(-0.05, min(irf_df$lower, na.rm = TRUE))

                irf_plot <- ggplot(irf_df, aes(x = horizon, y = estimate,
                                  fill = response, colour = response)) +
                  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
                  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
                  geom_line(linewidth = 0.8) +
                  scale_colour_manual(values = c(
                    "Producer Price Index" = "#2C6E8A",
                    "Consumer Price Index" = "#C45E3E",
                    "Import Price Index"   = "#4A9A6F"), 
                    drop = FALSE
                  ) +
                  scale_fill_manual(values = c(
                    "Producer Price Index" = "#2C6E8A",
                    "Consumer Price Index" = "#C45E3E",
                    "Import Price Index"   = "#4A9A6F"),
                    drop = FALSE
                  ) +
                  scale_x_continuous(breaks = seq(0, hor, by = 3), limits = c(0, hor), expand = c(0, 0)) +
                  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
                  coord_cartesian(ylim = c(y_min, NA)) +
                  labs(x      = "Months after shock",
                       y      = "Cumulative Response (%)",
                       title  = plot_title,
                       colour = NULL,
                       fill   = NULL) +
                  guides(
                      colour = guide_legend(
                        title = NULL,
                        override.aes = list(fill = NA, alpha = 1)
                      ),
                      fill = "none"
                ) +
                  theme_publication()

                ggsave(
                  filename = file_name,
                  plot     = irf_plot,
                  path     = file.path(here::here(), "irf"),
                  width    = 4,
                  height   = 2.5,
                  dpi      = 150,
                  device   = "png"
                )

                horizons <- seq(3, hor, by = 3)

                ####
                ## Generate PT Table
                ####

                pt_table <- irf_df %>%
                    filter(horizon %in% horizons) %>%
                    mutate(
                        se    = (upper - lower) / (2 * 1.96),
                        xr_cum = irf_results$irf[[imp]][horizon + 1, imp] * scale_factor * 100,
                        df = var_summary$varresult[[resp[1]]]$df[2]
                    ) %>%
                    select(horizon, response, estimate, se, lower, upper, df) %>%
                    arrange(response, horizon)

                ####
                ## Generate FEVD Table
                ####

                vd <- vars::fevd(var_model, n.ahead = hor)
                fevd_row_i <- tibble(horizon = horizons)
                fevd_dta <- lapply(resp, function(r) {
                  temp_col <- c(vd[[r]][horizons, imp])                 
                  }) %>%
                  setNames(resp) %>%
                  bind_cols()
                  fevd_df <- cbind(fevd_row_i, fevd_dta)

                ####
                ## Create Adjustment Speed Table
                ####
                  
                max_pt <- irf_df %>%
                  select(estimate, response) %>%
                  group_by(response) %>%
                  summarise(max = max(estimate))

                speed_table <- irf_df %>%
                              group_by(response) %>%
                              mutate(
                              speed_est = (estimate / max(estimate) * 100)
                              ) %>%
                              ungroup() %>%
                              mutate(estimate = estimate) %>%
                              select(horizon, response, speed_est, estimate) %>%
                              arrange(response, horizon)

                adj_plot <- ggplot(speed_table, aes(x = horizon, y = speed_est,
                                fill = response, colour = response)) +
                geom_line(linewidth = 0.8) +
                scale_colour_manual(values = c(
                  "Producer Price Index" = "#2C6E8A",
                  "Consumer Price Index" = "#C45E3E",
                  "Import Price Index"   = "#4A9A6F"),
                  drop = FALSE
                ) +
                scale_fill_manual(values = c(
                  "Producer Price Index" = "#2C6E8A",
                  "Consumer Price Index" = "#C45E3E",
                  "Import Price Index"   = "#4A9A6F"),
                  drop = FALSE
                ) +
                scale_x_continuous(breaks = seq(0, hor, by = 3), limits = c(0, hor), expand = c(0, 0)) +
                scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, NA), expand = c(0, NA)) +
                labs(x      = "Months after shock",
                      y      = "Adjustment (%)",
                      title  = plot_title,
                      colour = NULL,
                      fill   = NULL) +
                guides(
                      colour = guide_legend(
                        title = NULL,
                        override.aes = list(fill = NA, alpha = 1)
                      ),
                      fill = "none"
                ) +
                theme_publication()

              ggsave(
                filename = file_name_adj,
                plot     = adj_plot,
                path     = file.path(here::here(), "adj"),
                width    = 4,
                height   = 2.5,
                dpi      = 150,
                device   = "png"
              )

              horizons <- seq(3, hor, by = 3)
                  

return(list(irfplot = irf_plot, table = pt_table, model = summary(var_model), fevd = fevd_df, nobs = nrow(df_var), speed_tbl = speed_table, irfdf = irf_df, adj = adj_plot))

}



View(speed_table)

print(check$nobs)

look_missing <- full_filtered %>%
  select(time_period, oilshock, outputgap_dc, neer_sarb, m, ppi, cpi) %>%
  filter(if_any(everything(), is.na)) %>%
  select(time_period, everything())

View(look_missing)

sets <- c("full_filtered", "early", "low_inflation", "full_m", "early_m", "low_inflation_m")

irfplot <- list()
tables <- list()

decomp <- list()
adjplot <- list()

#sapply(c("plots", "tables", "decomp", "speed"), function(x) x <- list())

for (set in sets){
  if (set == "full_m"){
    temp_break  <- as.Date("2010-03-01")
  } else {
    temp_break = NA
  }

  if (grepl("_m$", set)) {
  temp_list <- c("oil", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")
  temp_resp <- c("m", "ppi", "cpi")
  with <- "with Import Prices "
  } else {
  temp_list <- c("oil", "outputgap_dc", "neer_sarb", "ppi", "cpi")
  temp_resp <- c("ppi", "cpi")
  with <- ""
  } 
  if (grepl("^full", set)){
  temp_name <- paste0("Full Sample ", with , "(1990-02 - 2022-12)")}
  else if (grepl("^early", set)) {
  temp_name <- paste0("Early Sample ", with , "(1990-02 - 2010-01)")
  } else{
    temp_name <- paste0("Low Inflation Sample ", with , "(2010-02 - 2022-12)")
  }

result <- plot_var(sample = get(set), 
      var_list = temp_list,
      name = temp_name, 
      resp = temp_resp, 
      n_lags = 12, 
      horizon = 18, 
      break_date = temp_break)

irfplot[[set]] <- result$irfplot

tables[[set]] <- result$table

decomp[[set]] <- result$fevd
adjplot[[set]] <- result$adj
}

pacman::p_load(ragg, grid, gridExtra)

plot_list  <- list(irf = irfplot, adj = adjplot)

combined_plots <- function(filename_irf = "irf_12lags.png", 
                            capt_irf = "Exchange Rate Measure is the Nominal Effective Exchange Rate.\nShaded regions reflect bootstrapped 95% confidence intervals.\nModel Calculated with 12 lags.",
                            filename_adj = "adj_12lags.png",
                            capt_adj = "Exchange Rate Measure is the Nominal Effective Exchange Rate.\n Adjustment speed equals the cumulative PT divided by maximum PT for a given response.\nOil shocks entered as endogenous\nModel Calculated with 12 lags."){
      for (type in c("irf", "adj")){

      if (type == "irf"){
        capt <- capt_irf
        filename <- filename_irf
        plots <- plot_list[["irf"]] 
      } else {
        capt <- capt_adj
        filename <- filename_adj
        plots <- plot_list[["adj"]] 
      }

      shared_legend <- cowplot::get_legend(
      plots[[4]] + theme(
        legend.text = element_text(family = "source_serif", size = 22, colour = "grey20"),
        legend.key.width  = unit(30, "pt"),
        legend.key.height = unit(30, "pt"),
        legend.spacing.x  = unit(85, "pt"),
        guides(colour = guide_legend(ncol = 3))

        )
      )
      plots <- lapply(plots, function(p) p + theme(legend.position = "none"))

      combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3) +
        plot_layout(guides = "keep", 
            heights = c(0.75, 0.75),   # equal row heights — reduce total by adjusting ggsave height
            widths  = c(0.75, 0.75, 0.75),
            axis_titles = "collect") +
        plot_annotation(
          # caption = capt,
          theme = theme(
            #  plot.caption  = element_text(hjust = 0, size = 9, family = "source_serif"),
            # legend.position = "bottom",
            # legend.text   = element_text(family = "source_serif"),
          #   legend.title  = element_text(family = "source_sans"),
          #   text         = element_text(family = "source_sans"),  # catches everything
           plot.title   = element_blank(),
          # axis.text    = element_text(family = "source_sans"),
          # axis.title   = element_text(family = "source_sans"),
          )
        ) &
        theme(
  plot.title   = element_blank(),
  axis.text    = element_text(family = "source_serif", size = 18),
  axis.title   = element_text(family = "source_serif", size = 22),
  # legend.text  = element_text(family = "source_serif"),
  # plot.caption = element_text(family = "source_serif"),
  plot.margin  = margin(11, 11, 11, 11)
)

      png(here::here("main", filename), width = 28, height = 20, units = "in", res = 150)
      showtext::showtext_begin()
      combined_grob <- patchworkGrob(combined_plot)

      col_labels <- arrangeGrob(
                textGrob(""),                          # blank spacer matching row_labels width
                textGrob("1990 - 2023", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                textGrob("1990 - 2010", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                textGrob("2010 - 2023", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                ncol = 4,
                widths = unit(c(1.5, 1 , 1, 1), c("cm", "null", "null", "null")),
                heights = unit(1, "cm")      # fixed height creates the gap
              )

      row_labels <- arrangeGrob(
        textGrob("Excl. Imports", rot = 90, hjust = 0.5, gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
        textGrob("Incl. Imports", rot = 90, hjust = 0.05, gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
        ncol = 1,
        widths = unit(1.5, "cm")
         )

          caption_lines <- strsplit(capt, "\n")[[1]]

          caption_grob <- arrangeGrob(
            grobs = lapply(caption_lines, function(line)
              textGrob(line, x = 0, hjust = 0,
                      gp = gpar(fontfamily = "source_serif", fontsize = 20))),
            ncol = 1
          )

          bottom_grob <- arrangeGrob(
            shared_legend,
            caption_grob,
            ncol = 1,
            heights = unit(c(1.6, 3.2), "cm")
          )

      labelled <- arrangeGrob(
        combined_grob,
        top  = col_labels,
        left = row_labels,
        bottom = bottom_grob
      )
      
      grid::grid.draw(labelled)
      showtext::showtext_end()
      dev.off()
      }
                            }

combined_plots()






sets_table <- c("full_filtered", "early", "low_inflation", "full_m", "early_m", "low_inflation_m")
sets_names <- c("1990 - 2023 ", "1990 - 2010 ", "2010 - 2023 ", "1990 - 2023", "1990 - 2010", "2010 - 2023")

combined_table <- function(irf = tables, fevd  = decomp, sets = sets_table, names = sets_names, horiz = 15){
  hor         <- as.numeric(horiz)
  table_list  <- list(irf = irf, fevd = fevd)

  for (type in c("irf", "fevd")){
    tbl <- table_list[[type]]

    # ── IRF branch ─────────────────────────────────────────────────────────────
    if (type == "irf"){
      caption     <- "Cumulative Exchange Rate Pass-Through at Selected Horizons with 12 lags"
      general     <- c("{Note:} Cumulative impulse responses to a one percent exchange rate depreciation.",
                       "        Bootstrapped standard errors in parentheses (500 replications).")
      kablab      <- "tab:erpt_p12"
      file        <- "p12_results.txt"
      resp_levels <- c("Import Price Index", "Producer Price Index", "Consumer Price Index", "Residual df")

      main <- lapply(seq_along(sets), function(i){
        tbl[[sets[i]]] %>%
          filter(horizon %in% seq(3, hor, by = 3)) %>%
          mutate(
            sample  = names[i],
            display = paste0(round(estimate, 2), " (", round(se, 2), ")")
          ) %>%
          select(sample, response, horizon, display)
      }) %>%
        bind_rows() %>%
        mutate(response = factor(response, levels = resp_levels)) %>%
        arrange(response, horizon) %>%
        pivot_wider(names_from = sample, values_from = display) %>%
        mutate(horizon = as.character(horizon))

      df_row <- lapply(seq_along(sets), function(i){
        tibble(sample = names[i], display = as.character(tbl[[sets[i]]]$df[1]))
      }) %>%
        bind_rows() %>%
        pivot_wider(names_from = sample, values_from = display) %>%
        mutate(horizon = "", response = factor("Residual df",
               levels = c("Import Price Index", "Producer Price Index",
                          "Consumer Price Index", "Residual df")))

      results_table <- bind_rows(main, df_row)

      if(hor > 15){
        results_txt <- results_table %>%
        select(-response) %>%
        rename("Horizon" = horizon) %>%
        kable(
          format   = "latex",
          booktabs = TRUE,
          linesep  = "",
          caption  = caption,
          label    = kablab
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) %>%
        add_header_above(
          c(" " = 1, "Excl. Imports" = 3, "Incl. Imports" = 3),
          bold = TRUE
        ) %>%
        pack_rows("Import Price Index",   1,  6) %>%
        pack_rows("Producer Price Index", 7,  12) %>%
        pack_rows("Consumer Price Index", 13, 18) %>%
        row_spec(6,  hline_after = TRUE) %>%
        row_spec(12, hline_after = TRUE) %>%
        row_spec(18, hline_after = TRUE) %>%
        footnote(
          general           = general,
          general_title     = "",
          footnote_as_chunk = FALSE,
          escape            = FALSE
        )
      }
      else {results_txt <- results_table %>%
        select(-response) %>%
        rename("Horizon" = horizon) %>%
        kable(
          format   = "latex",
          booktabs = TRUE,
          linesep  = "",
          caption  = caption,
          label    = kablab
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) %>%
        add_header_above(
          c(" " = 1, "Excl. Imports" = 3, "Incl. Imports" = 3),
          bold = TRUE
        ) %>%
        pack_rows("Import Price Index",   1,  5) %>%
        pack_rows("Producer Price Index", 6,  10) %>%
        pack_rows("Consumer Price Index", 11, 15) %>%
        row_spec(5,  hline_after = TRUE) %>%
        row_spec(10, hline_after = TRUE) %>%
        row_spec(15, hline_after = TRUE) %>%
        row_spec(20, hline_after = TRUE) %>%
        footnote(
          general           = general,
          general_title     = "",
          footnote_as_chunk = FALSE,
          escape            = FALSE
        )

      }

    # ── FEVD branch ────────────────────────────────────────────────────────────
    } else {
      caption      <- "Forecast Error Variance Decomposition at Selected Horizons with 12 lags"
      general      <- ""
      kablab       <- "tab:fevd_p12"
      file         <- "p12_fevd.txt"
      resp_levels  <- c("Import Price Index", "Producer Price Index", "Consumer Price Index")
      horizons_seq <- seq(3, hor, by = 3)
      period_levels <- c("1990 - 2023", "1990 - 2010", "2010 - 2023")

      all_data <- lapply(seq_along(sets), function(i){
        df <- tbl[[sets[i]]] %>%
          filter(horizon %in% horizons_seq) %>%
          rename(any_of(c(
            "Import Price Index"   = "m",
            "Producer Price Index" = "ppi",
            "Consumer Price Index" = "cpi"
          )))
        if (!"Import Price Index" %in% names(df)) df[["Import Price Index"]] <- NA_real_
        df %>%
          pivot_longer(-horizon, names_to = "response", values_to = "estimate") %>%
          mutate(
            sample     = trimws(names[i]),
            import_grp = if (grepl("_m$", sets[i])) "Incl. Imports" else "Excl. Imports"
          )
      }) %>% bind_rows()

      results_table <- all_data %>%
            mutate(
              response   = factor(response,   levels = resp_levels),
              import_grp = factor(import_grp, levels = c("Incl. Imports", "Excl. Imports")),
              sample     = factor(sample,     levels = period_levels),
              display    = round(estimate, 3)
            ) %>%
            arrange(response, sample, desc(import_grp)) %>%
            select(response, import_grp, sample, horizon, display) %>%
            pivot_wider(names_from = horizon, values_from = display, names_sort = TRUE) %>%
            group_by(response, sample) %>%
            mutate(sample_display = ifelse(row_number() == 1, as.character(sample), "")) %>%
            ungroup() %>%
            select(response, sample_display, import_grp, everything(), -sample)

      results_txt <- results_table %>%
        select(-response) %>%
        kable(
          format    = "latex",
          booktabs  = TRUE,
          linesep   = "",
          caption   = caption,
          label     = kablab,
          col.names = c("Sample", "Imports", as.character(horizons_seq))
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) %>%
        add_header_above(
          c(" " = 2, "Horizon (months)" = length(horizons_seq)),
          bold = TRUE
        ) %>%
        pack_rows("Import Price Index",    1,  6, bold = TRUE) %>%
        pack_rows("Producer Price Index",  7, 12, bold = TRUE) %>%
        pack_rows("Consumer Price Index", 13, 18, bold = TRUE) %>%
        row_spec(6,  hline_after = TRUE) %>%
        row_spec(12, hline_after = TRUE) %>%
        row_spec(18, hline_after = TRUE) %>%
        footnote(
          general           = general,
          general_title     = "",
          footnote_as_chunk = FALSE,
          escape            = FALSE
        )
    }

    writeLines(results_txt, here::here("main", file))
  }
}

combined_table(horiz = 18)

##############################################################################################################################################################################

############################################################################## Linear Projections ##############################################################################

##############################################################################################################################################################################


plot_lp <- function(sample     = full,
                    var_list   = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi"),
                    n_lags     = 9,
                    name       = "Full Sample",
                    imp        = "neer_sarb",
                    resp       = c("ppi", "cpi"),
                    horizon    = 18,
                    break_date = NA) {

  hor        <- as.numeric(horizon)
  name       <- as.character(name)
  oil_var    <- var_list[1]
  endog_vars <- setdiff(var_list, if (oil_var == "oilshock") "oilshock" else character(0))

  file_name     <- paste0(gsub(" ", "", tolower(name)), "_lp_p_", n_lags, ".png")
  file_name_adj <- paste0(gsub(" ", "", tolower(name)), "_lp_adj_p_", n_lags, ".png")
  plot_title    <- name

  # ── Pre-processing (mirrors plot_var) ────────────────────────────────────────
  df_raw <- sample
  if (!is.na(break_date))
    df_raw <- df_raw %>% mutate(d_imp = ifelse(time_period >= break_date, 1L, 0L))

  if (oil_var == "oilshock") {
    oil_mat <- sapply(0:n_lags, function(l) dplyr::lag(df_raw$oilshock, l))
    colnames(oil_mat) <- paste0("oil.l", 0:n_lags)
  }

  keep     <- df_raw$time_period >= lubridate::ymd("1990-02-01")
  endog_df <- df_raw[keep, ] %>% dplyr::select(all_of(endog_vars))

  exog_df <- if (oil_var == "oilshock") {
    d <- as.data.frame(oil_mat[keep, , drop = FALSE])
    if (!is.na(break_date)) d$d_imp <- df_raw$d_imp[keep]
    d
  } else if (!is.na(break_date)) {
    data.frame(d_imp = df_raw$d_imp[keep])
  } else {
    NULL
  }

  N <- nrow(endog_df)

  # ── Lag matrix (lags 1:n_lags of all endogenous) ─────────────────────────────
  lag_df <- bind_cols(lapply(1:n_lags, function(l) {
    m <- as.data.frame(lapply(endog_df, dplyr::lag, n = l))
    names(m) <- paste0(names(endog_df), ".l", l)
    m
}))

  # ── Step 1: Cholesky shock via first-stage OLS ───────────────────────────────
  # Regress imp on contemporaneous vars ordered above it + lags + exog
  imp_pos   <- which(endog_vars == imp)
  above_imp <- if (imp_pos > 1) endog_vars[seq_len(imp_pos - 1)] else character(0)

  fs_components <- list(
        tibble(y = endog_df[[imp]]),
        if (length(above_imp) > 0) endog_df[, above_imp, drop = FALSE] else NULL,
        lag_df,
        exog_df         # NULL is silently dropped by bind_cols when in a list
    )
    fs_df <- bind_cols(Filter(Negate(is.null), fs_components))


  valid_rows             <- complete.cases(fs_df)
  fs_fit                 <- lm(y ~ ., data = fs_df[valid_rows, ])
  shock_vec              <- rep(NA_real_, N)
  shock_vec[which(valid_rows)] <- residuals(fs_fit)
  scale_fac              <- 0.01 / sd(shock_vec, na.rm = TRUE)

  # ── Step 2: LP regressions ────────────────────────────────────────────────────
  label_map <- c(cpi = "Consumer Price Index",
                 ppi = "Producer Price Index",
                 m   = "Import Price Index")

  run_lp <- function(rv) {
    ests <- lowers <- uppers <- numeric(hor + 1)

    for (h in 0:hor) {
      # Cumulative dependent variable: y_{t+h} - y_{t-1}
      dep <- rowSums(sapply(0:h, function(j) dplyr::lead(endog_df[[rv]], j)), na.rm = TRUE)
      reg_components <- list(
        tibble(dep = dep, shock = shock_vec),
        lag_df,
        exog_df
    )
    reg_df <- bind_cols(Filter(Negate(is.null), reg_components)) %>% na.omit()

      fit     <- lm(dep ~ ., data = reg_df)
      nw_vcov <- tryCatch(
        sandwich::NeweyWest(fit, lag = max(h, 1), prewhite = FALSE, adjust = TRUE),
        error = function(e) vcov(fit)
      )

      beta          <- coef(fit)[["shock"]]
      se            <- sqrt(nw_vcov["shock", "shock"])
      ests[h + 1]   <- beta * scale_fac * 100
      lowers[h + 1] <- (beta - 1.96 * se) * scale_fac * 100
      uppers[h + 1] <- (beta + 1.96 * se) * scale_fac * 100
      dfs[h + 1] <- fit$df.residual
    }

    rv_label <- if (!is.na(label_map[rv])) unname(label_map[rv]) else rv
    data.frame(horizon = 0:hor, estimate = ests, lower = lowers, upper = uppers,
                df = dfs, response = rv_label)
  }

  irf_df <- bind_rows(lapply(resp, run_lp))

  # ── Plots (identical structure to plot_var) ───────────────────────────────────
  colour_vals <- c("Producer Price Index" = "#2C6E8A",
                   "Consumer Price Index" = "#C45E3E",
                   "Import Price Index"   = "#4A9A6F")

  y_min <- max(-0.05, min(irf_df$lower, na.rm = TRUE))

  irf_plot <- ggplot(irf_df, aes(x = horizon, y = estimate, fill = response, colour = response)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = colour_vals, drop = FALSE) +
    scale_fill_manual(values   = colour_vals, drop = FALSE) +
    scale_x_continuous(breaks = seq(0, hor, by = 3), limits = c(0, hor), expand = c(0, 0)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    coord_cartesian(ylim = c(y_min, NA)) +
    labs(x = "Months after shock", y = "Cumulative Response (%)",
         title = plot_title, colour = NULL, fill = NULL) +
    guides(colour = guide_legend(title = NULL, override.aes = list(fill = NA, alpha = 1)),
           fill = "none") +
    theme_publication()

  ggsave(file_name, irf_plot, path = file.path(here::here(), "irf"),
         width = 4, height = 2.5, dpi = 150, device = "png")

  horizons <- seq(3, hor, by = 3)

  pt_table <- irf_df %>%
    filter(horizon %in% horizons) %>%
    mutate(se = (upper - lower) / (2 * 1.96)) %>%
    dplyr::select(horizon, response, estimate, se, lower, upper) %>%
    arrange(response, horizon)

  speed_table <- irf_df %>%
    group_by(response) %>%
    mutate(speed_est = estimate / max(estimate) * 100) %>%
    ungroup() %>%
    dplyr::select(horizon, response, speed_est, estimate) %>%
    arrange(response, horizon)

  adj_plot <- ggplot(speed_table, aes(x = horizon, y = speed_est, fill = response, colour = response)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = colour_vals, drop = FALSE) +
    scale_fill_manual(values   = colour_vals, drop = FALSE) +
    scale_x_continuous(breaks = seq(0, hor, by = 3), limits = c(0, hor), expand = c(0, 0)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, NA), expand = c(0, NA)) +
    labs(x = "Months after shock", y = "Adjustment (%)", title = plot_title, colour = NULL, fill = NULL) +
    guides(colour = guide_legend(title = NULL, override.aes = list(fill = NA, alpha = 1)),
           fill = "none") +
    theme_publication()

  ggsave(file_name_adj, adj_plot, path = file.path(here::here(), "adj"),
         width = 4, height = 2.5, dpi = 150, device = "png")

  list(irfplot   = irf_plot,
       table     = pt_table,
       model     = fs_fit,    # first-stage fit, useful for shock diagnostics
       fevd      = NULL,      # not identified under LP
       nobs      = N,
       speed_tbl = speed_table,
       irfdf     = irf_df,
       adj       = adj_plot)
}


sets <- c("full_filtered", "early", "low_inflation", "full_m", "early_m", "low_inflation_m")

irfplot_lp <- list()
tables_lp <- list()

decomp_lp <- list()
adjplot_lp <- list()

#sapply(c("plots", "tables", "decomp", "speed"), function(x) x <- list())

for (set in sets){
  if (set == "full_m"){
    temp_break  <- as.Date("2010-03-01")
  } else {
    temp_break = NA
  }

  if (grepl("_m$", set)) {
  temp_list <- c("oil", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")
  temp_resp <- c("m", "ppi", "cpi")
  with <- "with Import Prices "
  } else {
  temp_list <- c("oil", "outputgap_dc", "neer_sarb", "ppi", "cpi")
  temp_resp <- c("ppi", "cpi")
  with <- ""
  } 
  if (grepl("^full", set)){
  temp_name <- paste0("Full Sample ", with , "(1990-02 - 2022-12)")}
  else if (grepl("^early", set)) {
  temp_name <- paste0("Early Sample ", with , "(1990-02 - 2010-01)")
  } else{
    temp_name <- paste0("Low Inflation Sample ", with , "(2010-02 - 2022-12)")
  }

result <- plot_lp(sample = get(set), 
      var_list = temp_list,
      name = temp_name, 
      resp = temp_resp, 
      imp = "neer_sarb",
      n_lags = 9, 
      horizon = 18, 
      break_date = temp_break)

irfplot_lp[[set]] <- result$irfplot

tables_lp[[set]] <- result$table

adjplot_lp[[set]] <- result$adj
}

pacman::p_load(ragg, grid, gridExtra)

plot_list_lp  <- list(irf = irfplot_lp, adj = adjplot_lp)

combined_plots_lp <- function(filename_irf = "irf_lp.png", 
                            capt_irf = "Exchange Rate Measure is the Nominal Effective Exchange Rate.\nShaded regions reflect bootstrapped 95% confidence intervals.\nModel Calculated with 9 lags using Linear Projections.",
                            filename_adj = "adj_lp.png",
                            capt_adj = "Exchange Rate Measure is the Nominal Effective Exchange Rate.\n Adjustment speed equals the cumulative PT divided by maximum PT for a given response.\nModel Calculated with 9 lags using Linear Projections."){
      for (type in c("irf", "adj")){

      if (type == "irf"){
        capt <- capt_irf
        filename <- filename_irf
        plots <- plot_list_lp[["irf"]] 
      } else {
        capt <- capt_adj
        filename <- filename_adj
        plots <- plot_list_lp[["adj"]] 
      }

      shared_legend <- cowplot::get_legend(
      plots[[4]] + theme(
        legend.text = element_text(family = "source_serif", size = 22, colour = "grey20"),
        legend.key.width  = unit(30, "pt"),
        legend.key.height = unit(30, "pt"),
        legend.spacing.x  = unit(85, "pt"),
        guides(colour = guide_legend(ncol = 3))

        )
      )
      plots <- lapply(plots, function(p) p + theme(legend.position = "none"))

      combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3) +
        plot_layout(guides = "keep", 
            heights = c(0.75, 0.75),   # equal row heights — reduce total by adjusting ggsave height
            widths  = c(0.75, 0.75, 0.75),
            axis_titles = "collect") +
        plot_annotation(
          # caption = capt,
          theme = theme(
            #  plot.caption  = element_text(hjust = 0, size = 9, family = "source_serif"),
            # legend.position = "bottom",
            # legend.text   = element_text(family = "source_serif"),
          #   legend.title  = element_text(family = "source_sans"),
          #   text         = element_text(family = "source_sans"),  # catches everything
           plot.title   = element_blank(),
          # axis.text    = element_text(family = "source_sans"),
          # axis.title   = element_text(family = "source_sans"),
          )
        ) &
        theme(
  plot.title   = element_blank(),
  axis.text    = element_text(family = "source_serif", size = 18),
  axis.title   = element_text(family = "source_serif", size = 22),
  # legend.text  = element_text(family = "source_serif"),
  # plot.caption = element_text(family = "source_serif"),
  plot.margin  = margin(11, 11, 11, 11)
)

      png(here::here("main", filename), width = 28, height = 20, units = "in", res = 150)
      showtext::showtext_begin()
      combined_grob <- patchworkGrob(combined_plot)

      col_labels <- arrangeGrob(
                textGrob(""),                          # blank spacer matching row_labels width
                textGrob("1990 - 2023", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                textGrob("1990 - 2010", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                textGrob("2010 - 2023", hjust = 0.5,
                gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
                ncol = 4,
                widths = unit(c(1.5, 1 , 1, 1), c("cm", "null", "null", "null")),
                heights = unit(1, "cm")      # fixed height creates the gap
              )

      row_labels <- arrangeGrob(
        textGrob("Excl. Imports", rot = 90, hjust = 0.5, gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
        textGrob("Incl. Imports", rot = 90, hjust = 0.05, gp = gpar(fontfamily = "source_serif", fontsize = 22, face = "bold")),
        ncol = 1,
        widths = unit(1.5, "cm")
         )

          caption_lines <- strsplit(capt, "\n")[[1]]

          caption_grob <- arrangeGrob(
            grobs = lapply(caption_lines, function(line)
              textGrob(line, x = 0, hjust = 0,
                      gp = gpar(fontfamily = "source_serif", fontsize = 20))),
            ncol = 1
          )

          bottom_grob <- arrangeGrob(
            shared_legend,
            caption_grob,
            ncol = 1,
            heights = unit(c(1.6, 3.2), "cm")
          )

      labelled <- arrangeGrob(
        combined_grob,
        top  = col_labels,
        left = row_labels,
        bottom = bottom_grob
      )
      
      grid::grid.draw(labelled)
      showtext::showtext_end()
      dev.off()
      }
                            }

combined_plots_lp()






sets_table <- c("full_filtered", "early", "low_inflation", "full_m", "early_m", "low_inflation_m")
sets_names <- c("1990 - 2023 ", "1990 - 2010 ", "2010 - 2023 ", "1990 - 2023", "1990 - 2010", "2010 - 2023")

combined_table_lp <- function(irf = tables_lp, sets = sets_table, names = sets_names, horiz = 18){
  hor         <- as.numeric(horiz)
      tbl <- tables_lp
      caption     <- "Cumulative Exchange Rate Pass-Through at Selected Horizons using Linear Projections."
      general     <- c("{Note:} Cumulative impulse responses to a one percent exchange rate depreciation.",
                       "        Bootstrapped standard errors in parentheses (500 replications).")
      kablab      <- "tab:erpt_lp"
      file        <- "lp_results.txt"
      resp_levels <- c("Import Price Index", "Producer Price Index", "Consumer Price Index", "Residual df")

      main <- lapply(seq_along(sets), function(i){
        tbl[[sets[i]]] %>%
          filter(horizon %in% seq(3, hor, by = 3)) %>%
          mutate(
            sample  = names[i],
            display = paste0(round(estimate / 100, 2), " (", round(se / 100, 2), ")")
          ) %>%
          select(sample, response, horizon, display)
      }) %>%
        bind_rows() %>%
        mutate(response = factor(response, levels = resp_levels)) %>%
        arrange(response, horizon) %>%
        pivot_wider(names_from = sample, values_from = display) %>%
        mutate(horizon = as.character(horizon))

      df_row <- lapply(seq_along(sets), function(i){
        tibble(sample = names[i], display = as.character(tbl[[sets[i]]]$df[1]))
      }) %>%
        bind_rows() %>%
        pivot_wider(names_from = sample, values_from = display) %>%
        mutate(horizon = "", response = factor("Residual df",
               levels = c("Import Price Index", "Producer Price Index",
                          "Consumer Price Index", "Residual df")))

      results_table <- bind_rows(main, df_row) 

      if(hor > 15){
        results_txt <- results_table %>%
        select(-response) %>%
        rename("Horizon" = horizon) %>%
        kable(
          format   = "latex",
          booktabs = TRUE,
          linesep  = "",
          caption  = caption,
          label    = kablab
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) %>%
        add_header_above(
          c(" " = 1, "Excl. Imports" = 3, "Incl. Imports" = 3),
          bold = TRUE
        ) %>%
        pack_rows("Import Price Index",   1,  6) %>%
        pack_rows("Producer Price Index", 7,  12) %>%
        pack_rows("Consumer Price Index", 13, 18) %>%
        row_spec(6,  hline_after = TRUE) %>%
        row_spec(12, hline_after = TRUE) %>%
        row_spec(18, hline_after = TRUE) %>%
        footnote(
          general           = general,
          general_title     = "",
          footnote_as_chunk = FALSE,
          escape            = FALSE
        )
      }
      else {results_txt <- results_table %>%
        select(-response) %>%
        rename("Horizon" = horizon) %>%
        kable(
          format   = "latex",
          booktabs = TRUE,
          linesep  = "",
          caption  = caption,
          label    = kablab
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) %>%
        add_header_above(
          c(" " = 1, "Excl. Imports" = 3, "Incl. Imports" = 3),
          bold = TRUE
        ) %>%
        pack_rows("Import Price Index",   1,  5) %>%
        pack_rows("Producer Price Index", 6,  10) %>%
        pack_rows("Consumer Price Index", 11, 15) %>%
        row_spec(5,  hline_after = TRUE) %>%
        row_spec(10, hline_after = TRUE) %>%
        row_spec(15, hline_after = TRUE) %>%
        row_spec(20, hline_after = TRUE) %>%
        footnote(
          general           = general,
          general_title     = "",
          footnote_as_chunk = FALSE,
          escape            = FALSE
        )

      }

    # # ── FEVD branch ────────────────────────────────────────────────────────────
    # } else {
    #   caption      <- "Forecast Error Variance Decomposition at Selected Horizons with 12 lags"
    #   general      <- ""
    #   kablab       <- "tab:fevd_p12"
    #   file         <- "p12_fevd.txt"
    #   resp_levels  <- c("Import Price Index", "Producer Price Index", "Consumer Price Index")
    #   horizons_seq <- seq(3, hor, by = 3)
    #   period_levels <- c("1990 - 2023", "1990 - 2010", "2010 - 2023")

    #   all_data <- lapply(seq_along(sets), function(i){
    #     df <- tbl[[sets[i]]] %>%
    #       filter(horizon %in% horizons_seq) %>%
    #       rename(any_of(c(
    #         "Import Price Index"   = "m",
    #         "Producer Price Index" = "ppi",
    #         "Consumer Price Index" = "cpi"
    #       )))
    #     if (!"Import Price Index" %in% names(df)) df[["Import Price Index"]] <- NA_real_
    #     df %>%
    #       pivot_longer(-horizon, names_to = "response", values_to = "estimate") %>%
    #       mutate(
    #         sample     = trimws(names[i]),
    #         import_grp = if (grepl("_m$", sets[i])) "Incl. Imports" else "Excl. Imports"
    #       )
    #   }) %>% bind_rows()

    #   results_table <- all_data %>%
    #         mutate(
    #           response   = factor(response,   levels = resp_levels),
    #           import_grp = factor(import_grp, levels = c("Incl. Imports", "Excl. Imports")),
    #           sample     = factor(sample,     levels = period_levels),
    #           display    = round(estimate, 3)
    #         ) %>%
    #         arrange(response, sample, desc(import_grp)) %>%
    #         select(response, import_grp, sample, horizon, display) %>%
    #         pivot_wider(names_from = horizon, values_from = display, names_sort = TRUE) %>%
    #         group_by(response, sample) %>%
    #         mutate(sample_display = ifelse(row_number() == 1, as.character(sample), "")) %>%
    #         ungroup() %>%
    #         select(response, sample_display, import_grp, everything(), -sample)

    #   results_txt <- results_table %>%
    #     select(-response) %>%
    #     kable(
    #       format    = "latex",
    #       booktabs  = TRUE,
    #       linesep   = "",
    #       caption   = caption,
    #       label     = kablab,
    #       col.names = c("Sample", "Imports", as.character(horizons_seq))
    #     ) %>%
    #     kable_styling(
    #       latex_options = c("HOLD_position", "scale_down"),
    #       full_width    = TRUE,
    #       font_size     = 8
    #     ) %>%
    #     add_header_above(
    #       c(" " = 2, "Horizon (months)" = length(horizons_seq)),
    #       bold = TRUE
    #     ) %>%
    #     pack_rows("Import Price Index",    1,  6, bold = TRUE) %>%
    #     pack_rows("Producer Price Index",  7, 12, bold = TRUE) %>%
    #     pack_rows("Consumer Price Index", 13, 18, bold = TRUE) %>%
    #     row_spec(6,  hline_after = TRUE) %>%
    #     row_spec(12, hline_after = TRUE) %>%
    #     row_spec(18, hline_after = TRUE) %>%
    #     footnote(
    #       general           = general,
    #       general_title     = "",
    #       footnote_as_chunk = FALSE,
    #       escape            = FALSE
    #     )
    # }

     writeLines(results_txt, here::here("main", file))
   }

combined_table_lp(horiz = 18)









for (set in sets){
  if (set == "full"){
    temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi")
    temp_name <- "Full Sample"
    temp_resp <- c("ppi", "cpi")
  }  else {
    temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")
    temp_name <- "Full Import Sample"
    temp_resp <- c("m", "ppi", "cpi")
  }
full_var <- var_table(dat = get(set), 
      var_list = temp_list,
      name = temp_name, 
      resp = temp_resp)
}


sd(full$uvi2, na.rm = TRUE)
sd(full$m_hist, na.rm = TRUE)

summary(full_var$var_model)
serial.test(full_var$var_model, lags.pt = 12, type = "PT.asymptotic")  # serial correlation
normality.test(full_var$var_model)                                       # normality of residuals
stability(full_var$var_model)                                            # eigenvalue stability

imports_var <- var_table(dat = full %>% filter(time_period > as.Date("1999-06-01"), time_period < as.Date("2023-01-01")), 
      var_list = c("oilshock", "outputgap_dc", "neer_sarb", "uvi34", "ppi", "cpi"),
      n_lags = 6, 
      name = "Model with Imports", 
      imp = "neer_sarb", 
      resp = c("ppi", "cpi", "uvi34"))

summary(imports_var$var_model)
      

cor(full$neer_sarb, full$usdzar_fred)

openxlsx::write.xlsx(full_var$table, 
           file      = here::here("Tables", "full_var_table.xlsx"),
           sheetName = "Pass-Through",
           rowNames  = FALSE)


filename_var <- file.path(here::here(), "Tables", "full_var_table.csv")
write.csv(full_var$table, filename_var)


################################################################################################################################################################################
########################################################################   Linear Projections Estimates  ######################################################################
################################################################################################################################################################################


lp_table <- function(set    = full,
                     var_list  = c("oilshock", "outputgap_dc", "neer_sarb", "ppi_manuf", "cpi"),
                     n_lags    = 12,
                     name      = "Full Sample",
                     imp       = "usdzar_fred",
                     resp      = c("ppi_manuf", "cpi"),
                     horizons  = c(3, 6, 12, 18, 24)) {

  oil <- var_list[1]   # exogenous, ordered first
  dem <- var_list[2]   # demand, ordered second (before the exchange rate)

  measure <- if (imp == "usdzar_fred")
    "Currency Measure is the USD/ZAR exchange Rate." else
    "Currency Measure is the SARB Nominal Effective Exchange Rate."

  # ── Data: contemporaneous + lags of every variable ──────────────────────
  df_var <- set %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  bd <- df_var
  for (v in var_list) for (l in seq_len(n_lags))
    bd[[paste0(v, ".l", l)]] <- dplyr::lag(df_var[[v]], l)

  lag_cols <- grep("\\.l[0-9]+$", names(bd), value = TRUE)

  # ── Recursive exchange-rate shock ───────────────────────────────────────
  # Orthogonalise the exchange rate wrt contemporaneous oil & demand and all
  # lags (ordering oil > demand > exchange rate). na.exclude pads the dropped
  # first n_lags rows with NA so 'shock' lines up with bd row-for-row.
  xr_fit <- lm(reformulate(c(oil, dem, lag_cols), response = imp),
               data = bd, na.action = na.exclude)
  bd$shock <- residuals(xr_fit)

  # ── Forward cumulative sum of a (differenced) series ────────────────────
  cum_lead <- function(x, h) {
    s <- x
    if (h > 0) for (j in seq_len(h)) s <- s + dplyr::lead(x, j)
    s
  }

  # ── One cumulative LP: returns cumulative coef + Newey-West SE ───────────
  lp_cum <- function(outcome, h) {
    d        <- bd
    d$cum_y  <- cum_lead(d[[outcome]], h)
    fit      <- lm(reformulate(c("shock", oil, dem, lag_cols), response = "cum_y"),
                   data = d)
    V        <- sandwich::NeweyWest(fit, lag = h + 1, prewhite = FALSE)
    c(coef = unname(coef(fit)["shock"]),
      se   = unname(sqrt(V["shock", "shock"])))
  }

  # ── Normalisation: shock that moves the exchange rate 1% on impact ──────
  xr_impact    <- unname(lp_cum(imp, 0)["coef"])     # ≈ 1 by construction
  scale_factor <- 0.01 / xr_impact

  # ── Exchange-rate cumulative response (PT denominator) ──────────────────
  xr <- vapply(horizons, function(h) lp_cum(imp, h), numeric(2))   # 2 x H

  # ── Price rows ──────────────────────────────────────────────────────────
  est <- dplyr::bind_rows(lapply(resp, function(r) {
    pr <- vapply(horizons, function(h) lp_cum(r, h), numeric(2))   # 2 x H

    gp <- pr["coef", ]; sp <- pr["se", ]    # price cumulative coef / se
    gx <- xr["coef", ]; sx <- xr["se", ]    # exchange-rate cumulative coef / se

    data.frame(
      resp     = r,
      horizon  = horizons,
      resp_val = gp * scale_factor,                              # scaled to 1% deprec.
      resp_se  = sp * scale_factor,
      pt_val   = gp / gx,                                        # scale-free ratio
      pt_se    = sqrt((1 / gx)^2 * sp^2 + (gp / gx^2)^2 * sx^2)  # delta method
    )
  }))

  # ── Format (identical layout to var_table) ──────────────────────────────
  est$Variable <- dplyr::case_when(
    est$resp %in% c("ppi_manuf", "ppi_full", "ppi_all") ~ "PPI",
    est$resp == "cpi"                                   ~ "CPI",
    est$resp %in% c("uvi34", "m_all", "m_manuf")         ~ "Import prices",
    TRUE                                                ~ est$resp)

  result <- data.frame(
    Variable = est$Variable,
    Horizon  = paste0("Month ", est$horizon),
    Response = paste0(round(est$resp_val * 100, 4), " (", round(est$resp_se * 100, 4), ")"),
    PT       = paste0(round(est$pt_val, 4),         " (", round(est$pt_se, 4), ")"),
    stringsAsFactors = FALSE
  )

  print(knitr::kable(result,
        col.names = c("Price stage", "Horizon", "Cumulative response (%)", "Pass-through"),
        align     = c("l", "l", "r", "r"),
        caption   = paste0("LP Cumulative ERPT — ", name, " (Newey-West HAC)")))

  invisible(list(table = result, est = est, scale_factor = scale_factor, xr_fit = xr_fit))
}


full_lp <- lp_table(sample = full, 
      var_list = c("oilshock", "outputgap_dc", "usdzar_fred", "ppi_manuf", "cpi"),
      name = "Full Sample", 
      resp = c("ppi_manuf", "cpi"))

View(full_lp$table)

openxlsx::write.xlsx(full_lp$table, 
           file      = here::here("Tables", "full_lp_table.xlsx"),
           sheetName = "Pass-Through",
           rowNames  = FALSE)


filename_lp <- file.path(here::here(), "Tables", "full_lp_table.csv")
write.csv(full_lp$table, filename_lp)

plot_lp <- function(dat   = new,
                    var_list = c("oilshock", "outputgap_dc", "usdzar_fred", "ppi_full", "cpi"),
                    n_lags   = 12,
                    name     = "New Sample",
                    imp      = var_list[3],
                    resp     = c("ppi_full", "cpi"),
                    hor      = 18) {

  name <- as.character(name)

  measure <- if (imp == "usdzar_fred"){
    "Currency Measure is the USD/ZAR exchange Rate.\nDashed lines reflect bootstrapped 95% confidence intervals."}
     else{
    "Currency Measure is the SARB Nominal Effective Exchange Rate.\nDashed lines reflect bootstrapped 95% confidence intervals."}

  file_name  <- paste0(gsub(" ", "", tolower(name)), "_lp_p_", as.character(n_lags), ".png")
  acfpath    <- file.path(here::here(), "acf", file_name)
  plot_title <- paste0("Impulse Response Functions using Linear Projections  of the ", name, " using p = ", n_lags)

  oil <- var_list[1]
  dem <- var_list[2]

  # ── Data: contemporaneous + lags ─────────────────────────────────────────
  df_var <- set %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  bd <- df_var
  for (v in var_list) for (l in seq_len(n_lags))
    bd[[paste0(v, ".l", l)]] <- dplyr::lag(df_var[[v]], l)

  lag_cols <- grep("\\.l[0-9]+$", names(bd), value = TRUE)

  # ── Recursive exchange rate shock ─────────────────────────────────────────
  xr_fit  <- lm(reformulate(c(oil, dem, lag_cols), response = imp),
                data = bd, na.action = na.exclude)
  bd$shock <- residuals(xr_fit)

  # ── ACF of residuals ──────────────────────────────────────────────────────
  resids_df <- bd %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  if ("oilshock" %in% var_list) {
    if (length(var_list) == 5) {
      colnames(resids_df) <- c("Oil Shock", "Demand Shock",
                               "Exchange Rate Shock", "PPI Shock", "CPI Shock")
    } else {
      colnames(resids_df) <- c("Oil Shock", "Demand Shock", "Exchange Rate Shock",
                               "Import Price Shock", "PPI Shock", "CPI Shock")
    }
  } else {
    if (length(var_list) == 5) {
      colnames(resids_df) <- c("Demand Shock", "Exchange Rate Shock",
                               "PPI Shock", "CPI Shock")
    } else {
      colnames(resids_df) <- c("Demand Shock", "Exchange Rate Shock",
                               "Import Price Shock", "PPI Shock", "CPI Shock")
    }
  }

  png(filename = acfpath, width = 1200, height = 800, res = 150)
  par(mfrow = c(2, 3))
  for (col in colnames(resids_df)) acf(resids_df[, col], main = col, lag.max = 24)
  dev.off()

  # ── Helpers identical to lp_table ─────────────────────────────────────────
  cum_lead <- function(x, h) {
    s <- x
    if (h > 0) for (j in seq_len(h)) s <- s + dplyr::lead(x, j)
    s
  }

  lp_cum <- function(outcome, h) {
    d       <- bd
    d$cum_y <- cum_lead(d[[outcome]], h)
    fit     <- lm(reformulate(c("shock", oil, dem, lag_cols), response = "cum_y"),
                  data = d)
    V       <- sandwich::NeweyWest(fit, lag = h + 1, prewhite = FALSE)
    c(coef  = unname(coef(fit)["shock"]),
      se    = unname(sqrt(V["shock", "shock"])))
  }

  # ── Normalisation ─────────────────────────────────────────────────────────
  xr_impact    <- unname(lp_cum(imp, 0)["coef"])
  scale_factor <- 0.01 / xr_impact

  # ── Compute IRF over all horizons 0:hor ───────────────────────────────────
  horizons <- 0:hor

  make_irf_df <- function(outcome, label) {
    pr <- vapply(horizons, function(h) lp_cum(outcome, h), numeric(2))

    coefs <- pr["coef", ] * scale_factor * 100
    ses   <- pr["se",   ] * scale_factor * 100

    data.frame(
      horizon  = horizons,
      estimate = coefs,
      lower    = coefs - 1.96 * ses,
      upper    = coefs + 1.96 * ses,
      response = label
    )
  }

  # ── Build irf_df ──────────────────────────────────────────────────────────
  if ("ppi" %in% var_list) {
    if ("m" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"),
        make_irf_df("m_all",   "Import Price Index"))
    } else if ("m_manuf" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"),
        make_irf_df("m_manuf", "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"))
    }
  } else if ("ppi_full" %in% var_list) {
    if ("uvi34" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_full", "Producer Price Index"),
        make_irf_df("cpi",      "Consumer Price Index"),
        make_irf_df("uvi34",    "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_full", "Producer Price Index"),
        make_irf_df("cpi",      "Consumer Price Index"))
    }
  } else if ("ppi_manuf" %in% var_list) {
    if ("uvi34" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_manuf", "Producer Price Index"),
        make_irf_df("cpi",       "Consumer Price Index"),
        make_irf_df("uvi34",     "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_manuf", "Producer Price Index"),
        make_irf_df("cpi",       "Consumer Price Index"))
    }
  }

  irf_df <- irf_df %>% dplyr::mutate(response = as.character(response))

  # ── Plot ──────────────────────────────────────────────────────────────────
  irf_plot <- ggplot(irf_df, aes(x = horizon, y = estimate,
                                  fill = response, colour = response)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    geom_line(aes(y = lower), linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(y = upper), linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c(
      "Producer Price Index"         = "#2C6E8A",
      "Consumer Price Index"          = "#C45E3E",
      "Import Price Index"  = "#4A9A6F")) +
    scale_fill_manual(values = c(
      "Producer Price Index"         = "#2C6E8A",
      "Consumer Price Index"          = "#C45E3E",
      "Import Prices Index"  = "#4A9A6F")) +
    scale_x_continuous(breaks = seq(0, hor, by = 3),
                       limits = c(0, hor),
                       expand = c(0, 0)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(x       = "Months after shock",
         y       = "Cumulative response (%)",
         title   = plot_title,
         caption = measure,
         colour  = NULL,
         fill    = NULL) +
    theme_publication()

  ggsave(
    filename = file_name,
    plot     = irf_plot,
    path     = file.path(here::here(), "irf"),
    width    = 10,
    height   = 6,
    dpi      = 150
  )

  invisible(list(irf_df = irf_df, irf_plot = irf_plot))
}
 
for (lag in c(3, 6, 12, 18)){

  for (set in sets){
    temp_resp <- c("m", "ppi", "cpi")

    if (set == "full"){
      temp_name <- "Full Sample (1990-02 - 2025-06)"
      temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi")
      temp_resp <- c("ppi", "cpi")
    }  else {temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m","ppi", "cpi")
            temp_resp <- c("m", "ppi", "cpi")
    }
    if (set == "full_filtered"){
      temp_name <- "Full Import Sample (1990-02 - 2022-12)"
      
    } else if (set == "low_inflation"){
      temp_name <- "2009-01 - 2022-12"
    } else if (set == "early"){
      temp_name <- "1990-02 - 2008-12"
    }
  plot_lp(sample = get(set), 
        var_list = temp_list,
        name = temp_name, 
        resp = temp_resp, 
        n_lags = lag)
  }
  }
