library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(stringr)
library(plotly)
library(RColorBrewer)
library(shinyWidgets)

data <- read_csv("data.csv")
couleurs_joueurs <- c("Cristiano Ronaldo" = "#003366", "Lionel Messi" = "#5a7ca6")

server <- function(input, output, session) {
  observeEvent(input$go_to_analysis, {
    updateTabsetPanel(session, "main_tabs", selected = "ANALYSE GLOBALE")
  })
  
  # Filtres dynamiques
  output$select_competition <- renderUI({
    comps <- sort(unique(data$Competition))
    pickerInput("filtre_competition", NULL, choices = c("Toutes", comps), selected = "Toutes",
                options = list(`style` = "btn-light", `live-search` = TRUE))
  })
  output$select_saison <- renderUI({
    saisons <- sort(unique(data$Season))
    pickerInput("filtre_saison", NULL, choices = c("Toutes", saisons), selected = "Toutes",
                options = list(`style` = "btn-light", `live-search` = TRUE))
  })
  
  # Badges filtres actifs
  output$badge_joueurs <- renderUI({
    req(input$joueurs_analyse)
    lapply(input$joueurs_analyse, function(j) {
      span(class = "badge-selected", j)
    })
  })
  output$badge_competition <- renderUI({
    if (!is.null(input$filtre_competition) && input$filtre_competition != "Toutes") {
      span(class = "badge-selected", input$filtre_competition)
    }
  })
  output$badge_saison <- renderUI({
    if (!is.null(input$filtre_saison) && input$filtre_saison != "Toutes") {
      span(class = "badge-selected", input$filtre_saison)
    }
  })
  
  # Données filtrées
  data_filtre <- reactive({
    df <- data
    if (!is.null(input$joueurs_analyse)) {
      df <- df %>% filter(Player %in% input$joueurs_analyse)
    }
    if (!is.null(input$filtre_competition) && input$filtre_competition != "Toutes") {
      df <- df %>% filter(Competition == input$filtre_competition)
    }
    if (!is.null(input$filtre_saison) && input$filtre_saison != "Toutes") {
      df <- df %>% filter(Season == input$filtre_saison)
    }
    df
  })
  
  # Résumé dynamique
  output$resume_dynamique <- renderUI({
    df <- data_filtre()
    n_buts <- nrow(df)
    n_joueurs <- length(unique(df$Player))
    n_clubs <- length(unique(df$Club))
    n_comp <- length(unique(df$Competition))
    n_saisons <- length(unique(df$Season))
    joueurs_txt <- if (n_joueurs == 2) "Messi et Ronaldo" else paste(df$Player[1])
    if (n_buts == 0) return(NULL)
    span(class = "resume-dynamique",
         paste0(joueurs_txt, " ont marqué ", n_buts, " but", ifelse(n_buts>1,"s",""),
                " dans ", n_clubs, " club", ifelse(n_clubs>1,"s",""),
                " et ", n_comp, " compétition", ifelse(n_comp>1,"s",""),
                " sur ", n_saisons, " saison", ifelse(n_saisons>1,"s",""), ".")
    )
  })
  
  # KPI animés et message si aucune donnée
  updateKPI <- function(id, value) {
    session$sendCustomMessage("animateKPI", id)
    value
  }
  output$kpi_buts <- renderText({
    v <- nrow(data_filtre())
    updateKPI("kpi_buts", v)
  })
  output$kpi_clubs <- renderText({
    v <- data_filtre() %>% pull(Club) %>% unique() %>% length()
    updateKPI("kpi_clubs", v)
  })
  output$kpi_competitions <- renderText({
    v <- data_filtre() %>% pull(Competition) %>% unique() %>% length()
    updateKPI("kpi_competitions", v)
  })
  output$kpi_types <- renderText({
    v <- data_filtre() %>% pull(Type) %>% unique() %>% length()
    updateKPI("kpi_types", v)
  })
  
  # Contenu dynamique selon le sous-onglet vertical sélectionné
  output$analyse_content <- renderUI({
    df <- data_filtre()
    if (nrow(df) == 0) {
      div(class = "no-data-msg", "Aucune donnée pour cette sélection. Essayez d'autres filtres !")
    } else {
      tab <- input$sousonglets_analyse
      if (tab == "evolution") {
        tagList(
          h4("Évolution interactive des buts par saison"),
          plotlyOutput("plot_buts_saison", width = "100%"),
          br(),
          h4("Timeline de progression des buts"),
          plotlyOutput("plot_timeline", width = "100%")
        )
      } else if (tab == "types") {
        tagList(
          h4("Répartition interactive des types de buts"),
          plotlyOutput("plot_types_buts", width = "100%")
        )
      } else if (tab == "passeurs") {
        tagList(
          h4("Meilleurs passeurs"),
          plotlyOutput("plot_top_passeurs", width = "100%")
        )
      } else if (tab == "adversaires") {
        tagList(
          h4("Clubs adverses préférés"),
          plotlyOutput("plot_top_adversaires", width = "100%")
        )
      } else if (tab == "tableau") {
        tagList(
          h4("Tableau détaillé des buts"),
          DTOutput("table_buts", width = "100%")
        )
      }
    }
  })
  
  output$table_buts <- DT::renderDT({
    data_filtre()
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    searchHighlight = TRUE
  ), class = 'display nowrap compact', extensions = 'Buttons')
  
  output$plot_buts_saison <- renderPlotly({
    df <- data_filtre() %>%
      group_by(Player, Season) %>%
      summarise(Buts = n(), .groups = "drop")
    plot_ly(df, x = ~Season, y = ~Buts, color = ~Player, colors = couleurs_joueurs,
            type = "bar") %>%
      layout(barmode = "group",
             title = "Évolution des buts par saison (interactif)",
             xaxis = list(title = "Saison"),
             yaxis = list(title = "Buts"))
  })
  
  output$plot_types_buts <- renderPlotly({
    df <- data_filtre() %>%
      group_by(Type) %>%
      summarise(Buts = n()) %>%
      filter(!is.na(Type) & Type != "")
    couleurs_rose <- colorRampPalette(c("#e84393", "#fd79a8", "#fab1a0", "#dfe6e9"))(max(3, nrow(df)))
    plot_ly(df, labels = ~Type, values = ~Buts, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = couleurs_rose)) %>%
      layout(title = "Répartition des types de buts")
  })
  
  output$plot_top_passeurs <- renderPlotly({
    df <- data_filtre() %>%
      filter(!is.na(Goal_assist) & Goal_assist != "") %>%
      group_by(Goal_assist, Player) %>%
      summarise(Passes = n(), .groups = "drop") %>%
      arrange(desc(Passes))
    top8 <- df %>%
      group_by(Goal_assist) %>%
      summarise(Total = sum(Passes)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 8) %>%
      pull(Goal_assist)
    df <- df %>% filter(Goal_assist %in% top8)
    plot_ly(
      df,
      x = ~Goal_assist,
      y = ~Passes,
      type = "bar",
      color = ~Player,
      colors = couleurs_joueurs
    ) %>%
      layout(barmode = "group",
             title = "Meilleurs passeurs",
             xaxis = list(title = "Joueur"),
             yaxis = list(title = "Passes décisives"))
  })
  
  output$plot_top_adversaires <- renderPlotly({
    df <- data_filtre() %>%
      group_by(Opponent, Player) %>%
      summarise(Buts = n(), .groups = "drop") %>%
      arrange(desc(Buts))
    top8 <- df %>%
      group_by(Opponent) %>%
      summarise(Total = sum(Buts)) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 8) %>%
      pull(Opponent)
    df <- df %>% filter(Opponent %in% top8)
    plot_ly(
      df,
      x = ~Opponent,
      y = ~Buts,
      type = "bar",
      color = ~Player,
      colors = couleurs_joueurs
    ) %>%
      layout(barmode = "group",
             title = "Clubs adverses préférés",
             xaxis = list(title = "Adversaire"),
             yaxis = list(title = "Buts"))
  })
  
  output$plot_timeline <- renderPlotly({
    df <- data_filtre() %>%
      filter(!is.na(Date)) %>%
      arrange(as.Date(Date, format="%Y-%m-%d")) %>%
      mutate(Cumul = 1:n())
    plot_ly(df, x = ~as.Date(Date, format="%Y-%m-%d"), y = ~Cumul, type = 'scatter', mode = 'lines+markers',
            color = ~Player, colors = couleurs_joueurs,
            text = ~paste("Adversaire:", Opponent, "<br>Type:", Type, "<br>Minute:", Minute)) %>%
      layout(title = "Timeline de progression des buts",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Total cumulé"))
  })
  
  # Variable réactive pour stocker le choix
  goat_choice <- reactiveVal(NULL)
  
  observeEvent(input$vote_cr7, {
    goat_choice("CRISTIANO RONALDO")
  })
  observeEvent(input$vote_messi, {
    goat_choice("LIONEL MESSI")
  })
  
  output$resultat_goat <- renderUI({
    choix <- goat_choice()
    if (is.null(choix)) return(NULL)
    if (choix == "CRISTIANO RONALDO") {
      tags$div(
        style = "margin-top:24px; color:#e84393; font-size:1.3em; font-family:'Oswald',sans-serif;",
        HTML('
        <span style="font-size:2em;vertical-align:middle;">🐐</span><br>
        <b>CRISTIANO RONALDO</b> est votre GOAT !<br>
        <span style="font-size:0.95em;color:#003366;">“Le travail, la puissance, la détermination. Le débat continue…”</span>
      ')
      )
    } else if (choix == "LIONEL MESSI") {
      tags$div(
        style = "margin-top:24px; color:#e84393; font-size:1.3em; font-family:'Oswald',sans-serif;",
        HTML('
        <span style="font-size:2em;vertical-align:middle;">🐐</span><br>
        <b>LIONEL MESSI</b> est votre GOAT !<br>
        <span style="font-size:0.95em;color:#003366;">“Le génie, la magie, la grâce. Le débat continue…”</span>
      ')
      )
    }
  })
  
  observeEvent(input$send_message, {
    session$sendCustomMessage('openMail', 'emmanueldjedje57@gmail.com')
  })
  
  
}
