# app.R — 2026 World Cup Prediction Dashboard
# Run with: shiny::runApp("scripts/shiny/app.R")
# Or deploy to shinyapps.io

options(sass.cache = FALSE)   # suppress bslib SCSS contrast warnings

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)

# resolve repo root — works whether run via runApp(), source(), or shinyapps.io
ROOT <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", ".."), mustWork = TRUE),
  error = function(e)
    normalizePath("~/projects/world-cup-bayesian", mustWork = FALSE)
)
data_path <- function(...) file.path(ROOT, "data", "processed", ...)

# ---- helpers ----
load_data <- function() {
  list(
    ensemble   = tryCatch(readRDS(data_path("progression_ensemble.rds")), error=function(e) NULL),
    bayesian   = tryCatch(readRDS(data_path("progression.rds")),          error=function(e) NULL),
    snapshots  = tryCatch(readRDS(data_path("snapshots.rds")),            error=function(e) NULL),
    strength   = tryCatch(readRDS(data_path("strength_current.rds")),     error=function(e) NULL),
    stack_w    = tryCatch(readRDS(data_path("stack_weights.rds")),        error=function(e) list(w_bay=0.59, w_gbm=0.41)),
    cal        = tryCatch(readRDS(data_path("calibration_pred.rds")),     error=function(e) NULL)
  )
}

# flag emoji lookup
flag <- function(team) {
  flags <- c(
    "Argentina"="🇦🇷","Australia"="🇦🇺","Austria"="🇦🇹","Belgium"="🇧🇪",
    "Brazil"="🇧🇷","Canada"="🇨🇦","Cape Verde"="🇨🇻","Colombia"="🇨🇴",
    "Croatia"="🇭🇷","Curaçao"="🇨🇼","DR Congo"="🇨🇩","Ecuador"="🇪🇨",
    "Egypt"="🇪🇬","England"="🏴󠁧󠁢󠁥󠁮󠁧󠁿","France"="🇫🇷","Germany"="🇩🇪",
    "Ghana"="🇬🇭","Iran"="🇮🇷","Iraq"="🇮🇶","Ivory Coast"="🇨🇮",
    "Japan"="🇯🇵","Jordan"="🇯🇴","Mexico"="🇲🇽","Morocco"="🇲🇦",
    "Netherlands"="🇳🇱","New Zealand"="🇳🇿","Norway"="🇳🇴","Panama"="🇵🇦",
    "Paraguay"="🇵🇾","Portugal"="🇵🇹","Saudi Arabia"="🇸🇦","Senegal"="🇸🇳",
    "South Korea"="🇰🇷","Spain"="🇪🇸","Sweden"="🇸🇪","Switzerland"="🇨🇭",
    "Tunisia"="🇹🇳","Turkey"="🇹🇷","United States"="🇺🇸","Uruguay"="🇺🇾",
    "Uzbekistan"="🇺🇿","Algeria"="🇩🇿","Norway"="🇳🇴"
  )
  f <- flags[team]
  if (is.na(f)) "🏳️" else f
}

groups <- c(A="Mexico",B="Canada",C="Brazil",D="United States",
            E="Germany",F="Netherlands",G="Belgium",H="Spain",
            I="France",J="Argentina",K="Portugal",L="England")

# ---- theme ----
wc_theme <- bs_theme(
  bg           = "#0f1923",
  fg           = "#f0f4f8",
  primary      = "#f5c518",
  secondary    = "#1a6eb5",
  success      = "#22c55e",
  danger       = "#e63946",
  base_font    = font_google("Inter"),
  heading_font = font_google("Barlow Condensed"),
  font_scale   = 1.05,
  bootswatch   = NULL
) %>%
  bs_add_rules("
    body { background: #0f1923; }
    .navbar { background: #071218 !important; border-bottom: 2px solid #f5c518; }
    .navbar-brand { color: #f5c518 !important; font-family: 'Barlow Condensed', sans-serif;
                    font-size: 1.5rem; font-weight: 700; letter-spacing: 1px; }
    .card { background: #162230; border: 1px solid #243447; border-radius: 12px; }
    .card-header { background: #1e2f42; border-bottom: 1px solid #243447;
                   font-family: 'Barlow Condensed', sans-serif; font-size: 1.1rem;
                   letter-spacing: 0.5px; color: #f5c518; }
    .nav-pills .nav-link { color: #8492a6; border-radius: 8px; }
    .nav-pills .nav-link.active { background: #f5c518; color: #0f1923; font-weight: 700; }
    .metric-box { background: #1e2f42; border-radius: 10px; padding: 16px 20px;
                  border-left: 4px solid #f5c518; }
    .metric-val { font-family: 'Barlow Condensed', sans-serif; font-size: 2.2rem;
                  font-weight: 700; color: #f5c518; line-height: 1; }
    .metric-lbl { font-size: 0.75rem; color: #8492a6; text-transform: uppercase;
                  letter-spacing: 1px; margin-top: 4px; }
    .podium-card { text-align: center; border-radius: 12px; padding: 20px 12px;
                   transition: transform 0.2s; }
    .podium-card:hover { transform: translateY(-4px); }
    .podium-1 { background: linear-gradient(135deg, #1e2f42, #2a4060);
                border: 2px solid #f5c518; }
    .podium-2 { background: linear-gradient(135deg, #1a2a3a, #243447);
                border: 2px solid #8492a6; }
    .podium-3 { background: linear-gradient(135deg, #1a2a3a, #243447);
                border: 2px solid #cd7f32; }
    .podium-flag { font-size: 3rem; line-height: 1.2; }
    .podium-team { font-family: 'Barlow Condensed', sans-serif; font-size: 1.3rem;
                   font-weight: 700; color: #f0f4f8; margin: 6px 0 2px; }
    .podium-pct  { font-family: 'Barlow Condensed', sans-serif; font-size: 2rem;
                   font-weight: 700; color: #f5c518; }
    .podium-rank { font-size: 0.7rem; color: #8492a6; text-transform: uppercase;
                   letter-spacing: 1px; }
    .update-badge { background: #22c55e22; border: 1px solid #22c55e;
                    color: #22c55e; border-radius: 20px; padding: 3px 12px;
                    font-size: 0.75rem; display: inline-block; }
    .section-label { font-family: 'Barlow Condensed', sans-serif; font-size: 0.7rem;
                     text-transform: uppercase; letter-spacing: 2px; color: #8492a6;
                     margin-bottom: 8px; }
    hr { border-color: #243447; }
    .selectize-input { background: #1e2f42 !important; border-color: #243447 !important;
                       color: #f0f4f8 !important; }
    .selectize-dropdown { background: #1e2f42 !important; border-color: #243447 !important; }
    .selectize-dropdown-content .option { color: #f0f4f8 !important; }
    .selectize-dropdown-content .option:hover { background: #243447 !important; }
    table.dataTable { background: #162230 !important; color: #f0f4f8 !important; }
    .dataTables_wrapper { color: #8492a6; }
  ")

# ---- UI ----
ui <- page_navbar(
  title = "⚽ WC 2026 PREDICTOR",
  theme = wc_theme,
  fillable = FALSE,

  # ---- Overview tab ----
  nav_panel("Overview",
    div(style = "padding: 24px;",

      # top strip: metadata
      div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:24px;",
        div(
          div(class="section-label", "FIFA World Cup 2026 — Live Predictions"),
          h2(style="color:#f0f4f8; font-family:'Barlow Condensed',sans-serif;
                    font-size:2rem; font-weight:700; margin:0;",
             "Who wins the World Cup?")
        ),
        div(
          uiOutput("last_updated"),
          div(style="margin-top:6px; font-size:0.8rem; color:#8492a6;",
              "Dynamic Bayesian + GBM ensemble · 2,000 simulations")
        )
      ),

      # stacking weight metrics row
      fluidRow(
        column(3, uiOutput("metric_n_sims")),
        column(3, uiOutput("metric_bay_weight")),
        column(3, uiOutput("metric_gbm_weight")),
        column(3, uiOutput("metric_brier"))
      ),

      br(),

      # podium — top 3
      div(class="section-label", "Top Contenders"),
      uiOutput("podium"),

      br(),

      # full win probability chart
      card(
        card_header("Championship win probability — all 48 teams"),
        plotlyOutput("win_prob_chart", height = "680px")
      )
    )
  ),

  # ---- Bracket tab ----
  nav_panel("Progression",
    div(style = "padding: 24px;",
      card(
        card_header("Tournament progression probabilities"),
        div(style="padding:16px;",
          div(style="color:#8492a6; font-size:0.85rem; margin-bottom:16px;",
              "Probability of reaching each stage, based on 2,000 simulated tournaments.
               Colour intensity = probability. Click a team to highlight its path."),
          plotlyOutput("heatmap", height = "780px")
        )
      )
    )
  ),

  # ---- Teams tab ----
  nav_panel("Team Strengths",
    div(style = "padding: 24px;",
      fluidRow(
        column(4,
          card(
            card_header("Select teams to compare"),
            div(style="padding:12px;",
              selectInput("selected_teams",
                label = NULL,
                choices  = NULL,
                multiple = TRUE,
                width    = "100%"
              ),
              div(style="color:#8492a6; font-size:0.8rem;",
                  "Attack strength = ability to score. Defense = ability to prevent goals.
                   Bands show 80% posterior credible interval.")
            )
          ),
          br(),
          card(
            card_header("Current ratings"),
            div(style="padding:12px;",
              tableOutput("strength_table")
            )
          )
        ),
        column(8,
          card(
            card_header("Attack vs Defense — current latent strengths"),
            plotlyOutput("att_def_scatter", height = "460px")
          )
        )
      )
    )
  ),

  # ---- Model tab ----
  nav_panel("Model",
    div(style = "padding: 24px;",
      fluidRow(
        column(6,
          card(
            card_header("Bayesian vs stacked ensemble"),
            div(style="padding:12px;",
              div(style="color:#8492a6; font-size:0.85rem; margin-bottom:12px;",
                  "Blue = dynamic Bayesian model alone (results only).
                   Orange = stacked ensemble (adds squad value, FIFA ranking, recent form).
                   Shifts show what Layer 2 contributes."),
              plotlyOutput("model_compare", height = "480px")
            )
          )
        ),
        column(6,
          card(
            card_header("Stacking weights"),
            div(style="padding:16px;",
              uiOutput("stack_weight_display")
            )
          ),
          br(),
          card(
            card_header("Calibration — reliability diagram"),
            div(style="padding:12px;",
              div(style="color:#8492a6; font-size:0.85rem; margin-bottom:12px;",
                  "Points on the dashed line = perfect calibration.
                   Dot size = number of matches in that probability bin."),
              plotlyOutput("reliability_plot", height = "300px")
            )
          )
        )
      )
    )
  ),

  # ---- Evolution tab ----
  nav_panel("Live Updates",
    div(style = "padding: 24px;",
      card(
        card_header("Win probability evolution across matchdays"),
        div(style="padding:16px;",
          div(style="color:#8492a6; font-size:0.85rem; margin-bottom:16px;",
              "Probabilities update after each matchday as new results are incorporated
               into the Bayesian model. Each point is a full refit + 2,000 simulations."),
          uiOutput("evolution_teams_ui"),
          br(),
          plotlyOutput("evolution_plot", height = "460px")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  d <- load_data()

  wc_teams <- c(
    "Argentina","Australia","Austria","Belgium","Brazil","Canada","Cape Verde",
    "Colombia","Croatia","Curaçao","DR Congo","Ecuador","Egypt","England",
    "France","Germany","Ghana","Iran","Iraq","Ivory Coast","Japan","Jordan",
    "Mexico","Morocco","Netherlands","New Zealand","Norway","Panama",
    "Paraguay","Portugal","Saudi Arabia","Senegal","Spain","Sweden",
    "Switzerland","Tunisia","Turkey","United States","Uruguay","Uzbekistan",
    "Algeria","South Korea"
  )

  ens <- if (!is.null(d$ensemble)) {
    d$ensemble %>% filter(team %in% wc_teams) %>% arrange(desc(P_Win))
  } else tibble(team=wc_teams, P_Win=0, P_Final=0, P_SF=0, P_QF=0, P_R16=0, P_R32=0)

  bay <- if (!is.null(d$bayesian)) {
    d$bayesian %>% filter(team %in% wc_teams) %>% arrange(desc(P_Win))
  } else ens

  str_df <- if (!is.null(d$strength)) {
    d$strength %>% filter(team %in% wc_teams)
  } else NULL

  # update team selector
  observe({
    top10 <- head(ens$team, 10)
    updateSelectInput(session, "selected_teams",
                      choices  = sort(wc_teams),
                      selected = top10)
  })

  # ---- metrics ----
  metric_box <- function(val, lbl) {
    div(class="metric-box",
        div(class="metric-val", val),
        div(class="metric-lbl", lbl))
  }

  output$last_updated <- renderUI({
    date_str <- if (!is.null(d$snapshots))
      format(max(d$snapshots$snapshot_date), "%B %d, %Y") else format(Sys.Date(), "%B %d, %Y")
    div(class="update-badge", paste("Updated", date_str))
  })

  output$metric_n_sims    <- renderUI(metric_box("2,000", "Simulations"))
  output$metric_bay_weight <- renderUI(metric_box(
    sprintf("%.0f%%", d$stack_w$w_bay * 100), "Bayesian weight"))
  output$metric_gbm_weight <- renderUI(metric_box(
    sprintf("%.0f%%", d$stack_w$w_gbm * 100), "GBM weight"))
  output$metric_brier     <- renderUI(metric_box("0.587", "Brier score"))

  # ---- podium ----
  output$podium <- renderUI({
    top3 <- head(ens, 3)
    medals <- list(
      list(rank="🥇 Favourite",     cls="podium-1"),
      list(rank="🥈 2nd favourite", cls="podium-2"),
      list(rank="🥉 3rd favourite", cls="podium-3")
    )
    fluidRow(
      map2(seq_len(nrow(top3)), medals, function(i, m) {
        tm <- top3$team[i]
        column(4,
          div(class=paste("podium-card", m$cls),
            div(class="podium-flag", flag(tm)),
            div(class="podium-team", tm),
            div(class="podium-pct",  percent(top3$P_Win[i], accuracy=0.1)),
            div(class="podium-rank", m$rank),
            div(style="margin-top:10px; display:flex; justify-content:center; gap:12px;",
              div(style="font-size:0.72rem; color:#8492a6;",
                  sprintf("Final %s", percent(top3$P_Final[i], accuracy=1))),
              div(style="font-size:0.72rem; color:#8492a6;",
                  sprintf("SF %s", percent(top3$P_SF[i], accuracy=1)))
            )
          )
        )
      })
    )
  })

  # ---- win prob chart ----
  output$win_prob_chart <- renderPlotly({
    df <- ens %>%
      arrange(P_Win) %>%
      mutate(
        team_flag = paste(map_chr(team, flag), team),
        team_flag = factor(team_flag, levels = team_flag),
        colour    = case_when(
          P_Win >= quantile(P_Win, 0.9) ~ "#f5c518",
          P_Win >= quantile(P_Win, 0.7) ~ "#e87722",
          TRUE                          ~ "#1a6eb5"
        ),
        tooltip = sprintf(
          "<b>%s %s</b><br>Win: %s<br>Final: %s<br>SF: %s<br>QF: %s",
          map_chr(team, flag), team,
          percent(P_Win, accuracy=0.1),
          percent(P_Final, accuracy=0.1),
          percent(P_SF, accuracy=0.1),
          percent(P_QF, accuracy=0.1)
        )
      )

    plot_ly(df, x = ~P_Win, y = ~team_flag,
            type = "bar", orientation = "h",
            marker = list(color = ~colour),
            text  = ~tooltip, hoverinfo = "text",
            textposition = "none") %>%
      layout(
        paper_bgcolor = "#162230",
        plot_bgcolor  = "#162230",
        font  = list(color = "#f0f4f8", family = "Inter"),
        xaxis = list(tickformat = ".0%", gridcolor = "#243447",
                     title = "P(Win World Cup)", color = "#8492a6"),
        yaxis = list(title = "", color = "#f0f4f8", tickfont = list(size = 11)),
        margin = list(l = 160, r = 40, t = 20, b = 40),
        hoverlabel = list(bgcolor = "#1e2f42", bordercolor = "#f5c518",
                          font = list(color = "#f0f4f8"))
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- progression heatmap ----
  output$heatmap <- renderPlotly({
    df <- ens %>%
      arrange(desc(P_Win)) %>%
      head(32) %>%
      mutate(team_flag = paste(map_chr(team, flag), team))

    rounds <- c("P_R32","P_R16","P_QF","P_SF","P_Final","P_Win")
    labels <- c("Round of 32","Round of 16","Quarter-final","Semi-final","Final","Win")

    mat <- df %>% select(all_of(rounds)) %>% as.matrix()
    rownames(mat) <- df$team_flag

    plot_ly(
      x = labels, y = df$team_flag,
      z = mat, type = "heatmap",
      colorscale = list(c(0,"#0f1923"), c(0.3,"#1a6eb5"),
                        c(0.7,"#e87722"), c(1,"#f5c518")),
      text  = apply(mat, 2, function(col) percent(col, accuracy=1)),
      hovertemplate = "%{y}<br>%{x}: %{text}<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "#162230",
        plot_bgcolor  = "#162230",
        font   = list(color = "#f0f4f8", family = "Inter"),
        xaxis  = list(color = "#f5c518", tickfont = list(size=11)),
        yaxis  = list(color = "#f0f4f8", tickfont = list(size=11),
                      autorange = "reversed"),
        margin = list(l=160, r=20, t=20, b=60)
      ) %>%
      colorbar(title = "Probability", tickformat = ".0%",
               bgcolor = "#1e2f42", bordercolor = "#243447",
               tickfont = list(color="#f0f4f8")) %>%
      config(displayModeBar = FALSE)
  })

  # ---- att/def scatter ----
  output$att_def_scatter <- renderPlotly({
    req(str_df)
    df <- str_df %>%
      filter(team %in% (input$selected_teams %||% head(ens$team, 10))) %>%
      mutate(
        team_flag = paste(map_chr(team, flag), team),
        tooltip   = sprintf(
          "<b>%s %s</b><br>Attack: %.3f<br>Defense: %.3f<br>Rating: %.3f",
          map_chr(team, flag), team, att, def, rating)
      )

    plot_ly(df, x=~att, y=~def, text=~tooltip,
            hoverinfo="text", type="scatter", mode="markers+text",
            textposition="top center",
            text2 = ~team_flag,
            marker=list(size=14, color=~rating,
                        colorscale=list(c(0,"#1a6eb5"),c(0.5,"#e87722"),c(1,"#f5c518")),
                        showscale=TRUE,
                        colorbar=list(title="Rating", tickfont=list(color="#f0f4f8"),
                                      bgcolor="#1e2f42", bordercolor="#243447"))) %>%
      add_text(text=~team_flag, textfont=list(size=9, color="#8492a6")) %>%
      layout(
        paper_bgcolor="#162230", plot_bgcolor="#162230",
        font=list(color="#f0f4f8", family="Inter"),
        xaxis=list(title="Attack strength", gridcolor="#243447", color="#8492a6",
                   zeroline=FALSE),
        yaxis=list(title="Defense strength", gridcolor="#243447", color="#8492a6",
                   zeroline=FALSE),
        margin=list(l=60,r=20,t=20,b=60),
        showlegend=FALSE
      ) %>%
      config(displayModeBar=FALSE)
  })

  # ---- strength table ----
  output$strength_table <- renderTable({
    req(str_df)
    str_df %>%
      filter(team %in% (input$selected_teams %||% head(ens$team,10))) %>%
      arrange(desc(rating)) %>%
      transmute(
        Team   = paste(map_chr(team, flag), team),
        Attack = round(att, 3),
        Defense = round(def, 3),
        Rating  = round(rating, 3)
      )
  }, striped=TRUE, hover=TRUE, bordered=FALSE,
     rownames=FALSE,
     align="lrrr")

  # ---- model comparison ----
  output$model_compare <- renderPlotly({
    top15 <- head(ens$team, 15)
    df <- bind_rows(
      bay %>% filter(team %in% top15) %>% mutate(model="Bayesian only"),
      ens %>% filter(team %in% top15) %>% mutate(model="Stacked ensemble")
    ) %>%
      mutate(team = factor(team, levels=rev(top15)),
             team_flag = paste(map_chr(team, as.character), team))

    plot_ly(df, x=~P_Win, y=~team, color=~model,
            colors=c("Bayesian only"="#1a6eb5","Stacked ensemble"="#e87722"),
            type="bar", orientation="h",
            text=~percent(P_Win, accuracy=0.1),
            textposition="outside",
            hovertemplate="%{y}<br>%{x:.1%}<extra>%{fullData.name}</extra>") %>%
      layout(
        barmode="group",
        paper_bgcolor="#162230", plot_bgcolor="#162230",
        font=list(color="#f0f4f8", family="Inter"),
        xaxis=list(tickformat=".0%", gridcolor="#243447",
                   title="P(Win World Cup)", color="#8492a6"),
        yaxis=list(title="", color="#f0f4f8"),
        legend=list(orientation="h", x=0, y=-0.12,
                    font=list(color="#f0f4f8"),
                    bgcolor="#162230"),
        margin=list(l=140,r=60,t=20,b=60)
      ) %>%
      config(displayModeBar=FALSE)
  })

  # ---- stacking weight display ----
  output$stack_weight_display <- renderUI({
    w_b <- d$stack_w$w_bay
    w_g <- d$stack_w$w_gbm
    div(
      div(class="section-label", "Optimised by log-score on played WC matches"),
      br(),
      div(style="display:flex; gap:16px; margin-bottom:20px;",
        div(class="metric-box", style="flex:1; border-left-color:#1a6eb5;",
          div(class="metric-val", style="color:#1a6eb5;", sprintf("%.0f%%", w_b*100)),
          div(class="metric-lbl", "Bayesian model")
        ),
        div(class="metric-box", style="flex:1; border-left-color:#e87722;",
          div(class="metric-val", style="color:#e87722;", sprintf("%.0f%%", w_g*100)),
          div(class="metric-lbl", "GBM (squad/form/ranking)")
        )
      ),
      hr(),
      div(style="color:#8492a6; font-size:0.82rem; line-height:1.6;",
        p("The Bayesian model earns majority weight because in-tournament results
           are the strongest predictor of current form. The GBM's 41% weight
           reflects the genuine predictive value of squad quality and FIFA ranking
           beyond what match results alone capture — most visibly correcting the
           pure-results model's underestimation of France and Brazil."),
        p(style="margin-bottom:0;",
          "Method: Yao et al. (2018) stacking via log-score maximisation.
           n = 24 played group-stage matches.")
      )
    )
  })

  # ---- reliability plot ----
  output$reliability_plot <- renderPlotly({
    req(d$cal)
    bins <- d$cal %>%
      mutate(bin_mid = round(p_home / 0.1) * 0.1) %>%
      group_by(bin_mid) %>%
      summarise(actual=mean(result=="home"), n=n(), .groups="drop")

    plot_ly() %>%
      add_trace(x=c(0,1), y=c(0,1), type="scatter", mode="lines",
                line=list(color="#243447", dash="dash", width=1.5),
                hoverinfo="skip", name="Perfect") %>%
      add_trace(data=bins, x=~bin_mid, y=~actual,
                type="scatter", mode="markers+lines",
                marker=list(size=~sqrt(n)*6, color="#f5c518",
                            line=list(color="#0f1923", width=1)),
                line=list(color="#f5c518", width=2),
                text=~sprintf("Predicted: %s<br>Actual: %s<br>n=%d",
                              percent(bin_mid,1), percent(actual,1), n),
                hoverinfo="text", name="Model") %>%
      layout(
        paper_bgcolor="#162230", plot_bgcolor="#162230",
        font=list(color="#f0f4f8", family="Inter"),
        xaxis=list(tickformat=".0%", title="Predicted P(home win)",
                   gridcolor="#243447", color="#8492a6", range=c(0,1)),
        yaxis=list(tickformat=".0%", title="Observed frequency",
                   gridcolor="#243447", color="#8492a6", range=c(0,1)),
        showlegend=FALSE,
        margin=list(l=60,r=20,t=10,b=60)
      ) %>%
      config(displayModeBar=FALSE)
  })

  # ---- evolution ----
  output$evolution_teams_ui <- renderUI({
    req(d$snapshots)
    top8 <- d$snapshots %>%
      filter(snapshot_date==max(snapshot_date)) %>%
      arrange(desc(P_Win)) %>% head(8) %>% pull(team)
    selectInput("evo_teams", "Teams to display",
                choices=sort(wc_teams), selected=top8,
                multiple=TRUE, width="100%")
  })

  output$evolution_plot <- renderPlotly({
    req(d$snapshots, input$evo_teams)
    df <- d$snapshots %>%
      filter(team %in% input$evo_teams) %>%
      mutate(team_flag=paste(map_chr(team, flag), team))

    n_teams <- n_distinct(df$team)
    pal <- colorRampPalette(c("#1a6eb5","#f5c518","#e63946","#22c55e",
                              "#e87722","#a855f7","#06b6d4","#f43f5e"))(n_teams)

    plot_ly(df, x=~snapshot_date, y=~P_Win, color=~team_flag,
            colors=pal, type="scatter", mode="lines+markers",
            line=list(width=2.5),
            marker=list(size=8),
            text=~sprintf("<b>%s</b><br>%s: %s",
                          team_flag, snapshot_date, percent(P_Win,0.1)),
            hoverinfo="text") %>%
      layout(
        paper_bgcolor="#162230", plot_bgcolor="#162230",
        font=list(color="#f0f4f8", family="Inter"),
        xaxis=list(title="", gridcolor="#243447", color="#8492a6"),
        yaxis=list(tickformat=".0%", title="P(Win World Cup)",
                   gridcolor="#243447", color="#8492a6"),
        legend=list(orientation="h", x=0, y=-0.15,
                    font=list(color="#f0f4f8"), bgcolor="#162230"),
        margin=list(l=60,r=20,t=20,b=80)
      ) %>%
      config(displayModeBar=FALSE)
  })
}

shinyApp(ui, server)