# Server functions

# Return uploaded file
upload_file <- function(file) {
  req(file)
  ext <- tools::file_ext(file$datapath)
  validate(
    need(ext %in% c("sas7bdat", "xls", "xlsx", "csv", "rds"), 
         "Please upload a SAS data, Excel, or CSV file.")
  )
  
  # Handle different file types
  if(ext == "sas7bdat") {
    haven::read_sas(file$datapath)
  } else if(ext %in% c("xls", "xlsx")) {
    readxl::read_excel(file$datapath) %>%
      mutate(across(where(is.POSIXt), as.Date))
  } else if(ext == "csv") {
    readr::read_csv(file$datapath, show_col_types = FALSE) %>%
      mutate(across(everything(), convert_csv_date))
  } else if(ext == "rds") {
    readRDS(file$datapath)
  }
}

# Convert dates (from uploaded CSV files)
convert_csv_date <- function(x) {
  if (grepl("\\d{1,2}\\/\\d{2}\\/\\d{4}", x[[1]])) {
    as.Date(x, format = "%m/%d/%Y")
  } else {
    x
  }
}

# Populate default selected variables
grep_var <- function(pattern, x) {
  grep(pattern, x, TRUE, value = TRUE)[1]
}
grepl_var <- function(pattern, x) {
  any(grepl(pattern, x, TRUE))
}

# Ensure "None" returns if pattern is not found
ifelse_var <- function(pattern, x) {
  if(grepl_var(pattern, x)) {
    grep_var(pattern, x)
  } else {
    "None"
  }
}

# Process uploaded files
process_file <- function(data, subj_var = NULL, label.rm = FALSE) {
  out <- data %>%
    # Convert blank entries to NA (if not date)
    mutate(across(!where(~lubridate::is.Date(.x) | lubridate::is.POSIXt(.x)),
                  ~replace(., which(.x == "" | .x == "NA"), NA))) %>%
    # Coerce subject variable to character
    mutate(across(any_of(subj_var), as.character))
  
  if(label.rm) {
    # Remove variable labels
    out <- haven::zap_label(out)
  } else if(!is.null(subj_var)) {
    # Keep subject variable label (as.character removes it)
    subj_label <- labelled::var_label(data[[subj_var]])
    labelled::var_label(out[[subj_var]]) <- subj_label
  }
  out
}
# Are the values nonnumeric?
nonnumeric <- function(x) {
  x %>%
    as.numeric() %>%
    is.na() %>%
    suppressWarnings()
}

# Variable labels from imported SAS files
var_labels <- function(x) {
  req(x)
  labels <- labelled::var_label(x, unlist = TRUE)
  # If label exists, add parentheses, otherwise leave as empty string
  purrr::map_chr(labels, ~replace(., which(.x != ""), paste0("(", .x, ")")))
}

# Table functions --------------------------------------------------------------

# Return number of distinct subjects
subjects_fn <- function(df, subj_var) {
  req(subj_var %in% names(df))
  n_distinct(df[[subj_var]])
}

# Return number of subjects in HTML form
subjects_HTML <- function(n) {
  HTML(paste0("<h4>Total subjects: ", n, "</h4>"))
}

# Create main data tables
data_table_fn <- function(df) {
  validate(
    need(is.data.frame(df), "Please upload a readable data set.")
  )
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  # Column widths for factor (character) columns
  # Otherwise the filter selects are too narrow
  # col_widths <- df[, colSums(is.na(df)) != nrow(df)] %>%
  #   select(where(is.factor)) %>%
  #   purrr::map_int(~max(nchar(as.character(.x)), na.rm = TRUE)) %>%
  #   purrr::map_dbl(~min(round(50 * sqrt(.x), -1), 250)) %>%
  #   purrr::map_chr(~paste0(.x, "px"))
  # names(col_widths) <- col_widths %>%
  #   purrr::imap(~match(.y, names(df)))
  # col_widths <- col_widths %>%
  #   purrr::imap(~list(width = .x, targets = as.numeric(.y))) %>%
  #   unname()
  
  DT::datatable(df,
                extensions = c("ColReorder", "FixedHeader"),
                options = list(
                  # autoWidth = TRUE,
                  # columnDefs = col_widths,
                  fixedHeader = TRUE,
                  colReorder = list(realtime = FALSE)),
                filter = "top")
}

# Missingness table (showing percentage as color bar)
# n is the column number to take as % missing
miss_table <- function(x, n) {
  x %>%
    DT::formatPercentage(n, digits = 2) %>%
    DT::formatStyle(n,
                    background = DT::styleColorBar(c(0, 1), "#99c3ee"),
                    backgroundSize = "100% 88%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
}

# Demographic table functions --------------------------------------------------

# Summarize n and percentage, then pivot the data
summarize_pivot_demo <- function(df, var, fill = NA) {
  df %>%
    reframe(
      n = n(),
      pct = round(n / unique(N) * 100, 1)
    ) %>%
    pivot_longer(cols = c(n, pct)) %>%
    unite(temp, all_of(var), name) %>%
    pivot_wider(names_from = temp, values_from = value, values_fill = fill)
}

# Calculate summary statistics
summarize_stats_demo <- function(df, trt, var, header = NULL, 
                                 stat_keep = c("n", "Mean (SD)", "Median", 
                                               "Q1, Q3", "Min, Max")) {
  df2 <- df %>%
    group_by(.data[[trt]]) %>%
    summarize(n = sum(!is.na(.data[[var]])), 
              d_mean = mean(.data[[var]], na.rm = TRUE),
              sd = sprintf("%.2f", sd(.data[[var]], na.rm = TRUE)), 
              max = max(.data[[var]], na.rm = TRUE),
              d_q3 = quantile(.data[[var]], probs = 0.75, type = 2, na.rm = TRUE), 
              d_median = median(.data[[var]], na.rm = TRUE),
              d_q1 = quantile(.data[[var]], probs = 0.25, type = 2, na.rm = TRUE), 
              min = min(.data[[var]], na.rm = TRUE)) %>%
    mutate(max = na_if(max, -Inf),
           min = na_if(min, Inf),
           across(starts_with("m"), ~ifelse(. %% 1 == 0, ., sprintf("%.1f", .))),
           across(starts_with("d"), ~sprintf("%.1f", .)),
           d_mean = ifelse(sd == "NA", "NA, NA", paste0(d_mean, " (", sd, ")")),
           d_q1 = paste0(d_q1, ", ", d_q3),
           min = paste0(min, ", ", max),) %>%
    select(-sd, -max, -d_q3) %>%
    rename("Mean (SD)" = d_mean, 
           Median = d_median, 
           "Q1, Q3" = d_q1, 
           "Min, Max" = min) %>%
    select(all_of(trt), all_of(stat_keep)) %>%
    t() %>%
    data.frame() %>%
    suppressWarnings()
  df2 <- mutate(df2, Variable = paste0("    ", rownames(df2)), .before = 1)
  rownames(df2) <- NULL
  if(is.null(header)) {
    df2[1, ] <- c(var, rep("", ncol(df2) - 1))
  } else {
    df2[1, ] <- c(header, rep("", ncol(df2) - 1))
  }
  levels <- sort(unique(df[[trt]]))
  names(df2) <- c("Variable", levels)
  df2
}

# Summarize categorical variables
charfreq_demo <- function(df, trt, var, header = NULL) {
  df2 <- df %>%
    group_by(.data[[trt]], .data[[var]]) %>%
    summarize_pivot_demo(trt, fill = 0)
  
  df2 <- df2 %>%
    mutate(across(ends_with("pct"), 
                  ~paste0("(", sprintf("%.1f", .), ")"))) %>%
    arrange(.data[[var]])
  
  levels <- sort(unique(df[[trt]]))
  req(levels)
  # Combine n and pct columns
  for(i in 1:length(levels)) {
    df2 <- df2 %>%
      unite(!!rlang::sym(as.character(levels[i])), 
            starts_with(as.character(levels[i])), 
            sep = " ")
  }
  names(df2) <- c("Variable", levels)
  df2 <- mutate(df2, Variable = paste0("    ", Variable))
  if(is.null(header)) {
    new_row <- c(var, rep("", ncol(df2) - 1))
  } else {
    new_row <- c(header, rep("", ncol(df2) - 1))
  }
  rbind(new_row, df2)
}

# Graph functions --------------------------------------------------------------

# Change default configuration of plots
custom_config <- function(p, text, ...) {
  plotly::config(p,
                 ...,
                 toImageButtonOptions = list(filename = text),
                 displaylogo = FALSE)
}

# Set color scale (interpolate colors if n is larger than Set2)
set_colors <- function(n) {
  if(n <= 3) {
    RColorBrewer::brewer.pal(3, "Set2")
  } else if(n <= 8) {
    RColorBrewer::brewer.pal(n, "Set2")
  } else {
    colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
  }
}

# Pie chart function
custom_pie <- function(df, params, label, value, title_text, 
                       legend_title, show_legend, download_text) {
  n <- length(params)
  n_row <- ceiling(n/3)
  row_pos <- sort(rep(0:n_row, 3))
  col_pos <- rep(c(0:2), n_row)
  p <- plotly::plot_ly()
  for(i in 1:n) {
    df_param <- df[df[[1]] == params[i], ]
    p <- p %>% 
      plotly::add_pie(
        data = df_param, 
        labels = ~.data[[label]], 
        values = ~.data[[value]],
        name = params[i],
        title = list(text = params[i], font = list(size = 14)),
        marker = list(line = list(color = "white", width = 1)),
        textposition = "inside",
        textinfo = "label+value+percent",
        hoverinfo = "label+value+percent",
        textfont = list(color = "black"),
        hoverlabel = list(font = list(color = "black")),
        domain = list(row = row_pos[i], column = col_pos[i])
      )
  }
  p %>% 
    plotly::layout(
      title = title_text,
      colorway = RColorBrewer::brewer.pal(8, "Set2"),
      showlegend = show_legend,
      legend = list(title = list(text = legend_title)),
      grid = list(rows = n_row, columns = 3)
    ) %>%
    custom_config(download_text)
}

# Summary functions ------------------------------------------------------------

# Return summary data frame for quantitative/numeric variables
numeric_summary <- function(df, group, value) {
  df %>%
    group_by(.data[[group]]) %>%
    summarize(n = n(),
              Minimum = min(.data[[value]], na.rm = TRUE),
              Maximum = max(.data[[value]], na.rm = TRUE),
              Mean = mean(.data[[value]], na.rm = TRUE),
              Median = median(.data[[value]], na.rm = TRUE),
              "Std Dev" = sd(.data[[value]], na.rm = TRUE)) %>%
    mutate(Minimum = ifelse(Minimum == Inf, NA, Minimum),
           Maximum = ifelse(Maximum == -Inf, NA, Maximum)) %>%
    suppressWarnings()
}

# Return logical vector of outlier status
find_outliers <- function(data) {
  lowerq <- quantile(data, type = 2)[2]
  upperq <- quantile(data, type = 2)[4]
  iqr <- IQR(data, type = 2)
  lower_fence <- lowerq - (1.5 * iqr)
  upper_fence <- upperq + (1.5 * iqr)
  data < lower_fence | data > upper_fence
}

# Flag if min/max in summary is an outlier
outlier_flag <- function(df, summary, var, value) {
  vars <- sort(unique(df[[var]]))
  n <- length(vars)
  min_flag <- vector(length = n)
  max_flag <- vector(length = n)
  
  for(i in 1:n) {
    data <- filter(df, .data[[var]] == vars[i])
    summary_i <- summary[i, ]
    outlier_vec <- find_outliers(data[[value]])
    outlier_points <- data[[value]][which(outlier_vec)]
    min_flag[i] <- summary_i$Minimum %in% outlier_points
    max_flag[i] <- summary_i$Maximum %in% outlier_points
  }
  tibble(min_flag, max_flag)
}

# Create summary boxplot
summary_boxplot <- function(df, var, value, legend_title, show_points) {
  n <- n_distinct(df[[var]])
  p <- plotly::plot_ly(
    df, 
    x = ~.data[[value]], 
    color = ~.data[[var]], 
    type = "box",
    hoverinfo = "x",
    colors = set_colors(n),
    hoverlabel = list(font = list(color = "black")),
    pointpos = 0
  )
  # Show individual points if option is selected
  if(show_points) {
    p <- plotly::style(
      p, 
      boxpoints = "all",
      marker = list(opacity = 0.7),
      jitter = 0.7
    )
  }
  p %>%
    plotly::layout(
      xaxis = list(domain = c(0.1, 1),
                   title = value),
      yaxis = list(autorange = "reversed"),
      legend = list(
        title = list(text = paste0("<b>", legend_title, "</b>"))
      )
    ) %>%
    custom_config("-summary_plot", scrollZoom = TRUE)
}

# Function factory to get integer breaks (based on scales::breaks_pretty)
breaks_integer <- function(n = 5, ...) {
  function(x) {
    floor(pretty(x, n, ...))
  }
}

# Create summary bar chart
create_barchart <- function(df, var, value) {
  df <- df[df[[1]] == var, ]
  g <- ggplot(df, aes(x = n,
                      y = reorder(.data[[value]], 
                                  desc(.data[[value]])),
                      fill = .data[[value]])) +
    scale_x_continuous(breaks = breaks_integer()) +
    geom_col(show.legend = FALSE) +
    labs(title = var, x = NULL, y = NULL) +
    theme_light(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  height <- n_distinct(df[[value]]) * 40
  renderPlot(g, height = max(140, height), res = 96)
}

# Current date and time (at app start) for downloaded tables
runtime_table <- toupper(format(Sys.time(), format = "%d%b%Y:%H:%M:%S"))

# Standard format for downloaded tables from Table Builder
report_table <- function(df, title, footnote, group = NULL) {
  tbl <- df %>%
    reporter::create_table(width = 9) %>%
    reporter::column_defaults(vars = 2:ncol(df), align = "center") %>%
    # Add blank line below title
    reporter::spanning_header(from = "1", to = as.character(ncol(df)), 
                              underline = FALSE, standard_eval = TRUE)
  
  # Add subgroup label
  if(!is.null(group)) {
    tbl <- tbl %>%
      reporter::page_by(group, label = "", align = "center") %>%  
      reporter::define(group, visible = FALSE, standard_eval = TRUE)
  }
  rpt <- reporter::create_report(font = "Courier") %>%
    reporter::set_margins(1, 1, 1, 1) %>%
    reporter::add_content(tbl) %>%
    reporter::page_header(right = "Page [pg] of [tpg]") %>%
    reporter::footnotes(footnote, 
                        blank_row = "none",
                        borders = "top",
                        footer = TRUE) %>%
    reporter::page_footer(right = runtime_table,
                          blank_row = "none")
  if(title != "") {
    rpt <- rpt %>%
      reporter::titles(title,
                       borders = "bottom")
  }
  rpt
}

# Table Builder functions ------------------------------------------------------

# Order treatments if TRTN is selected
order_trt <- function(df, trt, trtn) {
  if(trtn == "None") {
    df %>%
      mutate(trt = .data[[trt]])
  } else {
    trt_num <- unique(df[[trtn]])
    trt_name <- unique(df[[trt]])
    trt_levels <- trt_name[order(trt_num, trt_name)]
    df %>%
      mutate(trt = factor(.data[[trt]], 
                          levels = trt_levels, ordered = TRUE))
  }
}

# Standard format for gt table
gt_table_format <- function(gtt, tbl, title, footnote) {
  gtt <- gtt %>%
    gt::tab_style(
      gt::cell_text(whitespace = "pre"),
      gt::cells_body()
    ) %>%
    gt::tab_style(
      gt::cell_text(align = "center"),
      gt::cells_row_groups()
    ) %>%
    gt::cols_align(
      "center",
      names(tbl)[-1]
    )
  if(title != "") {
    title <- gsub("\\n", "<br>", title)
    gtt <- gtt %>%
      gt::tab_header(gt::md(title))
  }
  if(footnote != "") {
    footnote <- gsub("\\n", "<br>", footnote)
    gtt <- gtt %>%
      gt::tab_footnote(gt::md(footnote))
  }
  gtt
}
