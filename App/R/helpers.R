# =============================================================================
#  Helper functions: colours, layout pieces, charts, tables and insights
#  (Shiny loads every file in the R/ folder automatically.)
# =============================================================================

# ---- Colours ------------------------------------------------------------------
# Serious, colour-blind friendly colours (dark blue / dark orange / teal ...).
# "coral" is kept as a name but is now a deep red: it marks "high risk".
pal <- list(
  navy = "#051650", teal = "#2C7A7B", teal_dark = "#1A5251",
  teal_light = "#7FC9C8", coral = "#B03A2E", gold = "#D9822B", grey = "#6B7280",
  blue = "#1F4E79"
)
pal_series <- c("#1F4E79", "#D9822B", "#2C7A7B", "#7A3E9D", "#6B7280",
                "#B03A2E", "#4E79A7", "#59A14F", "#9C755F", "#B07AA1")

# Height (pixels) of every chart, so all cards in a row are the same size
chart_h <- 360

pct <- function(x, acc = 0.1) {
  if (length(x) == 0 || all(is.na(x))) return("n/a")
  scales::percent(as.numeric(x), accuracy = acc)
}
num <- function(x) format(round(x), big.mark = ",", scientific = FALSE)


# =============================================================================
# 1. Layout pieces
# =============================================================================

# Gradient page banner
page_banner <- function(title, subtitle) {
  div(class = "page-banner", h2(title), p(subtitle))
}

# "About this page" card
page_intro <- function(...) {
  div(class = "page-intro", h5("About this page"), ...)
}

# Big KPI inside the navy strip
kpi_hero <- function(icon, output_id, label) {
  div(class = "kpi-hero",
      div(class = "kpi-icon", bs_icon(icon)),
      div(class = "kpi-value", textOutput(output_id, inline = TRUE)),
      div(class = "kpi-label", label))
}

# Small gradient KPI card
kpi_card <- function(title, output_id, sub_id, icon, style = "teal") {
  div(class = paste("kpi-card", paste0("kpi-", style)),
      div(class = "kpi-icon", bs_icon(icon)),
      div(class = "kpi-title", title),
      div(class = "kpi-value", textOutput(output_id, inline = TRUE)),
      div(class = "kpi-sub", textOutput(sub_id, inline = TRUE)))
}

# Chart or table box with a navy header and an insight footer
dash_box <- function(title, icon, ..., insight_id = NULL, teal = FALSE) {
  card(
    class = paste("dash-box", if (teal) "teal"),
    full_screen = TRUE,
    card_header(bs_icon(icon), title),
    card_body(...),
    if (!is.null(insight_id))
      card_footer(class = "insight", bs_icon("lightbulb-fill"),
                  uiOutput(insight_id, inline = TRUE))
  )
}


# =============================================================================
# 2. Calculations
# =============================================================================
# All numbers are sample counts and simple proportions.

# Unemployment rate per category
rate_by <- function(df, var) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(category = .data[[var]]) %>%
    summarise(youth = n(), rate = mean(unemployed), .groups = "drop")
}

# Distribution (share) of a variable, optionally within groups
share_by <- function(df, var, by = NULL) {
  df <- filter(df, !is.na(.data[[var]]))
  if (!is.null(by)) df <- df %>% filter(!is.na(.data[[by]])) %>% group_by(group = .data[[by]])
  df %>%
    group_by(category = .data[[var]], .add = TRUE) %>%
    summarise(youth = n(), .groups = "drop_last") %>%
    mutate(share = youth / sum(youth)) %>%
    ungroup()
}

overall_rate <- function(df) mean(df$unemployed)


# =============================================================================
# 3. Charts (plotly). Transparent backgrounds so they work in light and dark.
# =============================================================================

style_plot <- function(p, x_title = NULL, y_title = NULL, x_pct = FALSE,
                       y_pct = FALSE, legend = TRUE) {
  axis <- function(title, is_pct) list(
    title = if (is.null(title)) "" else title, tickformat = if (is_pct) ".0%" else NULL,
    gridcolor = "rgba(128,128,128,0.18)", zeroline = FALSE,
    automargin = TRUE, color = "#7B8794")
  p %>%
    layout(
      xaxis = axis(x_title, x_pct), yaxis = axis(y_title, y_pct),
      margin = list(l = 10, r = 25, t = 10, b = 10),
      font = list(family = "Inter, Segoe UI, Arial", size = 13, color = "#7B8794"),
      # Legend ABOVE the chart, so it never covers the x-axis title
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
      showlegend = legend,
      hoverlabel = list(bgcolor = pal$navy, font = list(color = "#fff")),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    chart_buttons()
}

# Small toolbar on each chart: zoom, reset and "download as PNG"
chart_buttons <- function(p) {
  config(p, displaylogo = FALSE,
         modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                    "toggleSpikelines", "hoverClosestCartesian",
                                    "hoverCompareCartesian"),
         toImageButtonOptions = list(format = "png", scale = 2))
}

dashed_line <- function(x) {
  list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
       line = list(dash = "dash", color = pal$grey, width = 1.5))
}

# Horizontal bars: unemployment rate per category (dashed line = overall)
rate_chart <- function(df, var, sort = FALSE) {
  d <- rate_by(df, var)
  if (sort) d <- arrange(d, rate) %>% mutate(category = factor(category, levels = category))
  else d <- mutate(d, category = fct_rev(category))
  overall <- overall_rate(df)
  d$colour <- ifelse(d$rate > overall, pal$coral, pal$blue)   # red = above overall, blue = below

  plot_ly(d, x = ~rate, y = ~category, type = "bar", orientation = "h",
          marker = list(color = ~colour), text = ~pct(rate), textposition = "outside",
          customdata = ~youth,
          hovertemplate = "<b>%{y}</b><br>Unemployment: %{x:.1%}<br>Sample: %{customdata:,} youth<extra></extra>") %>%
    layout(shapes = list(dashed_line(overall)),
           xaxis = list(range = c(0, max(d$rate, overall) * 1.3))) %>%
    style_plot(x_title = "Unemployment rate", x_pct = TRUE, legend = FALSE)
}

# Grouped bars: unemployment rate of x, split by a second variable
rate_split_chart <- function(df, x, by) {
  d <- df %>%
    filter(!is.na(.data[[x]]), !is.na(.data[[by]])) %>%
    group_by(category = .data[[x]], group = .data[[by]]) %>%
    summarise(youth = n(), rate = mean(unemployed), .groups = "drop")
  plot_ly(d, x = ~category, y = ~rate, color = ~group, type = "bar",
          colors = pal_series[seq_len(n_distinct(d$group))], customdata = ~youth,
          text = ~pct(rate), textposition = "outside", cliponaxis = FALSE,
          hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1%}<br>Sample: %{customdata:,}<extra></extra>") %>%
    layout(barmode = "group", yaxis = list(range = c(0, max(d$rate) * 1.2))) %>%
    style_plot(y_title = "Unemployment rate", y_pct = TRUE)
}

# Horizontal bars: share of youth in each category
share_chart <- function(df, var, colour = pal$teal, top = NULL, ordered = FALSE) {
  d <- share_by(df, var)
  if (ordered) {
    d$category <- fct_rev(droplevels(d$category))     # keep natural order
  } else {
    d <- arrange(d, share)
    if (!is.null(top)) d <- tail(d, top)
    d$category <- factor(d$category, levels = d$category)
  }
  plot_ly(d, x = ~share, y = ~category, type = "bar", orientation = "h",
          marker = list(color = colour), text = ~pct(share), textposition = "outside",
          customdata = ~youth,
          hovertemplate = "<b>%{y}</b><br>Share: %{x:.1%}<br>Sample: %{customdata:,}<extra></extra>") %>%
    layout(xaxis = list(range = c(0, max(d$share) * 1.3))) %>%
    style_plot(x_title = "Share of youth", x_pct = TRUE, legend = FALSE)
}

# Clustered (side-by-side) bars: share of each category of var, within each group.
# Example: duration of unemployment (x-axis), one bar for women and one for men.
clustered_chart <- function(df, var, by) {
  d <- share_by(df, var, by)
  n <- n_distinct(d$group)
  plot_ly(d, x = ~category, y = ~share, color = ~group, type = "bar",
          colors = pal_series[seq_len(n)],
          text = ~pct(share, 1), textposition = "outside", cliponaxis = FALSE,
          hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1%}<extra></extra>") %>%
    layout(barmode = "group", yaxis = list(range = c(0, max(d$share) * 1.2))) %>%
    style_plot(y_title = "Share within group", y_pct = TRUE)
}

# Donut chart
donut_chart <- function(df, var) {
  d <- share_by(df, var)
  plot_ly(d, labels = ~category, values = ~youth, type = "pie", hole = .55,
          marker = list(colors = pal_series, line = list(color = "#FFFFFF", width = 2)),
          textinfo = "percent", textfont = list(color = "#FFFFFF", size = 14), sort = FALSE,
          hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>") %>%
    layout(legend = list(orientation = "h", y = -0.1),
           paper_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Inter, Segoe UI, Arial", size = 13, color = "#7B8794")) %>%
    chart_buttons()
}


# =============================================================================
# 4. Summary tables with margins (plain Bootstrap-style HTML tables)
# =============================================================================

rate_cell <- function(rate, max_rate) {
  div(class = "rate-cell",
      span(pct(rate)),
      div(class = "rate-track",
          div(class = "rate-bar", style = sprintf("width:%.0f%%", 100 * rate / max_rate))))
}

# One-way table: Employed | Unemployed | Total | Rate | Share, with a Total row
margin_table <- function(df, var) {
  d <- df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(category = .data[[var]]) %>%
    summarise(emp   = sum(employment_status == "Employed"),
              unemp = sum(employment_status == "Unemployed"), .groups = "drop") %>%
    mutate(total = emp + unemp, rate = unemp / total, share = total / sum(total))
  tot <- d %>% summarise(emp = sum(emp), unemp = sum(unemp)) %>%
    mutate(total = emp + unemp, rate = unemp / total, share = 1)
  max_rate <- max(d$rate)

  rows <- lapply(seq_len(nrow(d)), function(i) tags$tr(
    tags$td(as.character(d$category[i])),
    tags$td(class = "num", num(d$emp[i])),
    tags$td(class = "num", num(d$unemp[i])),
    tags$td(class = "num margin", num(d$total[i])),
    tags$td(class = "num", rate_cell(d$rate[i], max_rate)),
    tags$td(class = "num", pct(d$share[i]))
  ))
  tagList(
    div(class = "table-wrap", tags$table(class = "sum-table",
      tags$thead(tags$tr(tags$th("Category"), tags$th(class = "num", "Employed"),
                         tags$th(class = "num", "Unemployed"), tags$th(class = "num", "Total"),
                         tags$th(class = "num", "Unemp. rate"), tags$th(class = "num", "Share"))),
      tags$tbody(rows),
      tags$tfoot(tags$tr(tags$td("Total"), tags$td(class = "num", num(tot$emp)),
                         tags$td(class = "num", num(tot$unemp)), tags$td(class = "num", num(tot$total)),
                         tags$td(class = "num", rate_cell(tot$rate, max_rate)),
                         tags$td(class = "num", "100%")))
    )),
    div(class = "table-note",
        paste("Counts are youth in the labour force (LFS 2025 sample).",
              "Last row and 'Total' column are the margins."))
  )
}

# Colour for a rate cell (blue = low, red = high)
heat_colour <- function(rate, lo, hi) {
  t <- if (hi > lo) (rate - lo) / (hi - lo) else 0.5
  col <- grDevices::colorRamp(c("#9FC5E8", "#F7E1B5", "#E8A598"))(t)
  sprintf("background:rgba(%d,%d,%d,.85);color:#1E293B", round(col[1]), round(col[2]), round(col[3]))
}

# Two-way table of unemployment rates with row and column margins
rate_matrix_table <- function(df, x, by) {
  d <- df %>% filter(!is.na(.data[[x]]), !is.na(.data[[by]]))
  cell <- d %>% group_by(r = .data[[x]], c = .data[[by]]) %>%
    summarise(rate = mean(unemployed), n = n(), .groups = "drop")
  row_m <- d %>% group_by(r = .data[[x]]) %>% summarise(rate = mean(unemployed), n = n())
  col_m <- d %>% group_by(c = .data[[by]]) %>% summarise(rate = mean(unemployed), n = n())
  all_rate <- mean(d$unemployed)
  lo <- min(cell$rate); hi <- max(cell$rate)
  cols <- levels(droplevels(d[[by]])); rws <- levels(droplevels(d[[x]]))

  show <- function(rate, n, margin = FALSE) {
    if (length(rate) == 0) return(tags$td(class = "num muted", "-"))
    tags$td(class = paste("num", if (margin) "margin"),
            title = paste("Sample:", num(n)),
            if (margin) pct(rate) else span(class = "heat", style = heat_colour(rate, lo, hi), pct(rate)))
  }
  body <- lapply(rws, function(r) {
    tags$tr(tags$td(r),
            lapply(cols, function(cc) { z <- cell[cell$r == r & cell$c == cc, ]; show(z$rate, z$n) }),
            { z <- row_m[row_m$r == r, ]; show(z$rate, z$n, TRUE) })
  })
  tagList(
    div(class = "table-wrap", tags$table(class = "sum-table",
      tags$thead(tags$tr(tags$th(""), lapply(cols, function(cc) tags$th(class = "num", cc)),
                         tags$th(class = "num", "All"))),
      tags$tbody(body),
      tags$tfoot(tags$tr(tags$td("All"),
                         lapply(cols, function(cc) { z <- col_m[col_m$c == cc, ]; show(z$rate, z$n, TRUE) }),
                         tags$td(class = "num", pct(all_rate))))
    )),
    div(class = "table-note", "Cells show unemployment rates. 'All' row and column are the margins.",
        "Hover a cell to see its sample size.")
  )
}


# =============================================================================
# 5. Insight sentences
# =============================================================================

# "X has the highest rate (..), Y the lowest (..)"
insight_rate <- function(df, var, label) {
  d <- rate_by(df, var) %>% filter(youth >= 30)
  if (nrow(d) < 2) return(HTML("Not enough data for this selection."))
  hi <- d[which.max(d$rate), ]; lo <- d[which.min(d$rate), ]
  HTML(sprintf(
    "Among %s groups, <b>%s</b> has the highest unemployment (<b>%s</b>) and <b>%s</b> the lowest (<b>%s</b>). Overall rate: <b>%s</b>.",
    label, hi$category, pct(hi$rate), lo$category, pct(lo$rate), pct(overall_rate(df))))
}

# "The largest group is X (..%)"
insight_share <- function(df, var, who = "youth") {
  d <- share_by(df, var)
  if (nrow(d) == 0) return(HTML("No data for this selection."))
  top <- d[which.max(d$share), ]
  HTML(sprintf("The largest group is <b>%s</b>, covering <b>%s</b> of %s.",
               top$category, pct(top$share), who))
}
