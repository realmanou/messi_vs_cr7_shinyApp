library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinyBS)

ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    primary = "#003366",
    secondary = "#e84393",
    base_font = font_google("Montserrat"),
    heading_font = font_google("Oswald")
  ),
  tags$audio(
    id = "champions_audio",
    src = "champions_league.mp3",
    type = "audio/mp3",
    autoplay = NA,
    loop = NA,
    style = "display:none;"
  ),
  tags$head(
    tags$style(HTML("
      html, body {
        width: 100vw !important;
        height: 100vh !important;
        margin: 0 !important;
        padding: 0 !important;
        overflow-x: hidden !important;
        background: #fff;
      }
      .btn-group .btn {
    font-family: 'Oswald', sans-serif;
    font-size: 1.13em;
    padding: 10px 22px;
    border-radius: 15px !important;
    margin: 0 7px 7px 0;
    background: #fff !important;
    color: #003366 !important;
    border: 2px solid #e84393 !important;
    box-shadow: 0 1px 4px #eee;
    transition: background 0.2s, color 0.2s, border 0.2s;
  }
  .btn-group .btn.active, .btn-group .btn:active, .btn-group .btn:focus {
    background: #e84393 !important;
    color: #fff !important;
    border: 2px solid #003366 !important;
    box-shadow: 0 2px 8px #dfe6e9;
  }
  .btn-group .btn:hover {
    background: #003366 !important;
    color: #fff !important;
    border: 2px solid #e84393 !important;
  }
      .container, .container-fluid, .fluidPage, .tab-content, .tab-pane, .tabbable, .navbar, .main-container {
        width: 100vw !important;
        min-height: 100vh !important;
        margin: 0 !important;
        padding: 0 !important;
        box-sizing: border-box !important;
      }
      #accueil_content {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      .fade-in {
        opacity: 0;
        transition: opacity 2.5s ease-in;
      }
      .fade-in.visible {
        opacity: 1;
      }
      .nav-tabs {
        background-color: #003366 !important;
        border-radius: 0 !important;
        padding-left: 10px;
        padding-top: 5px;
        padding-bottom: 5px;
        margin-bottom: 0;
      }
      .nav-tabs .nav-link {
        color: #fff !important;
        font-weight: bold;
        font-family: 'Oswald', sans-serif;
        border-radius: 0 !important;
        transition: color 0.2s, background 0.2s;
      }
      .nav-tabs .nav-link.active,
      .nav-tabs .nav-item.show .nav-link,
      .nav-tabs .nav-link:focus,
      .nav-tabs .nav-link:active {
        color: #003366 !important;
        background-color: #e84393 !important;
        border: 1px solid #e84393 !important;
        border-bottom-color: transparent !important;
        border-radius: 0 !important;
      }
      .nav-tabs .nav-link:not(.active):hover {
        background-color: #e84393 !important;
        color: #003366 !important;
        border-radius: 0 !important;
      }
      /* Navlist vertical custom */
      .nav-stacked > li > a, .nav-pills > li > a {
        border-radius: 8px !important;
        margin-bottom: 7px;
        font-family: 'Oswald', sans-serif;
        font-size: 1.08em;
        color: #003366 !important;
        background: #fff !important;
        border: 2px solid #e84393 !important;
        transition: background 0.2s, color 0.2s;
      }
      .nav-stacked > li.active > a,
      .nav-stacked > li > a.active,
      .nav-stacked > li > a:focus,
      .nav-stacked > li > a:hover {
        background: #e84393 !important;
        color: #fff !important;
        border-color: #003366 !important;
      }
      /* Bandeau filtres */
      .filter-banner {
        background: linear-gradient(90deg, #e84393 0%, #003366 100%);
        border-radius: 18px;
        box-shadow: 0 2px 12px #eee;
        padding: 18px 30px 12px 30px;
        margin-bottom: 25px;
        display: flex;
        flex-direction: row;
        align-items: center;
        gap: 35px;
        justify-content: center;
      }
      .filter-block {
        display: flex;
        flex-direction: column;
        align-items: center;
        min-width: 170px;
      }
      .filter-block .fa-2x {
        margin-bottom: 6px;
      }
      .filter-label {
        font-family: 'Oswald', sans-serif;
        font-size: 1.08em;
        color: #fff;
        margin-bottom: 7px;
        font-weight: bold;
        letter-spacing: 1px;
      }
      .badge-selected {
        background: #fff;
        color: #e84393;
        font-weight: bold;
        border-radius: 12px;
        padding: 3px 11px;
        font-size: 1em;
        margin-top: 4px;
        margin-bottom: 7px;
        box-shadow: 0 1px 4px #eee;
      }
      /* KPI en cartes */
      .kpi-row {
        display: flex;
        flex-direction: row;
        gap: 18px;
        margin-bottom: 25px;
        justify-content: center;
      }
      .kpi-card {
        background: #fff;
        border: 2px solid #e84393;
        border-radius: 14px;
        box-shadow: 0 2px 8px #eee;
        padding: 16px 18px 10px 18px;
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 8px;
        min-width: 170px;
        min-height: 90px;
        position: relative;
        transition: box-shadow 0.2s, border-color 0.2s;
      }
      .kpi-card:hover {
        border-color: #003366;
        box-shadow: 0 4px 16px #ccc;
      }
      .kpi-title {
        font-family: 'Oswald', sans-serif;
        font-size: 1.1em;
        color: #003366;
        font-weight: bold;
        margin-bottom: 0;
        margin-right: 10px;
      }
      .kpi-value {
        font-size: 2em;
        color: #e84393;
        font-family: 'Montserrat', sans-serif;
        font-weight: bold;
        margin-left: auto;
        transition: color 0.4s;
      }
      .kpi-value.animated {
        color: #003366;
        animation: kpiPop 0.5s;
      }
      @keyframes kpiPop {
        0% { transform: scale(1.1); color: #e84393; }
        50% { transform: scale(1.25); color: #003366; }
        100% { transform: scale(1); color: #e84393; }
      }
      .kpi-help {
        position: absolute;
        top: 7px;
        right: 10px;
        color: #e84393;
        cursor: pointer;
      }
      .kpi-card .fa-question-circle {
        font-size: 1.1em;
      }
      /* Résumé dynamique */
      .resume-dynamique {
        font-family: 'Montserrat', sans-serif;
        font-size: 1.13em;
        color: #003366;
        background: #f5f6fa;
        border-radius: 12px;
        padding: 10px 20px;
        margin-bottom: 18px;
        box-shadow: 0 1px 4px #eee;
        text-align: center;
        font-weight: 500;
      }
      /* Message no data */
      .no-data-msg {
        color: #e84393;
        font-size: 1.25em;
        font-family: 'Oswald', sans-serif;
        margin: 40px 0 20px 0;
        text-align: center;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        setTimeout(function() {
          $('#accueil_content').addClass('visible');
        }, 800);
      });
      Shiny.addCustomMessageHandler('animateKPI', function(id) {
        var el = document.getElementById(id);
        if (el) {
          el.classList.remove('animated');
          void el.offsetWidth;
          el.classList.add('animated');
        }
      });
    "))
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("ACCUEIL",
             tags$div(
               style = "position:relative; min-height:100vh; overflow:hidden;",
               tags$img(
                 src = "messi_vs_ronaldo.jpg",
                 style = "
            position: absolute;
            top: 0; left: 0;
            width: 100%; height: 100%;
            object-fit: cover;
            z-index: 0;
            opacity: 0.45;"
               ),
               tags$div(
                 style = "position:absolute; top:0; left:0; width:100%; height:100%; background:linear-gradient(to bottom, rgba(0,0,0,0.5), rgba(0,0,0,0.7)); z-index:1;"
               ),
               tags$div(
                 id = "accueil_content",
                 class = "fade-in",
                 style = "position:relative; z-index:2; text-align:center; padding:90px 0 60px 0; color:white;",
                 h1("RONALDO VS MESSI : LE DUEL DES BUTEURS", style = "font-size:3em; font-weight:bold;"),
                 h3("COMPARAISON OFFICIELLE EXCLUSIVEMENT SUR LES BUTS DES JOUEURS", style = "color:#e84393;"),
                 p("⚽ Note : Cette application compare uniquement les buts inscrits par chaque joueur. Les autres aspects de leur carrière ne sont pas analysés ici.", style = "font-size:1.2em; margin-top:30px;"),
                 tags$div(
                   style = "max-width:700px; margin:auto; background:rgba(255,255,255,0.92); color:#003366; padding:25px; box-shadow:0 1px 4px #ccc; margin-top:35px;",
                   p("Plongez dans l’univers statistique de Lionel Messi et Cristiano Ronaldo : chaque but, chaque club, chaque compétition, chaque exploit est ici passé au crible, pour que vous puissiez explorer, comparer et débattre avec des chiffres.

Filtrez par saison, compétition, adversaire ou type de but. Visualisez leur évolution, découvrez leurs clubs fétiches, analysez leurs passeurs décisifs et explorez toutes les facettes de leur rivalité légendaire."),
                   tags$ul(
                     tags$li("Cartes de résumé interactives"),
                     tags$li("Tableaux dynamiques et graphiques modernes"),
                     tags$li("Filtres avancés par joueur, club, compétition")
                   ),
                   tags$div(
                     style = "text-align:center; margin-top:25px;",
                     actionButton("go_to_analysis", "VOIR L'ANALYSE GLOBALE",
                                  style = "background:#e84393; color:white; font-size:18px; border-radius:0; padding:10px 30px;")
                   )
                 )
               )
             )
    ),
    tabPanel("ANALYSE GLOBALE",
             # Bandeau filtres
             div(class = "filter-banner",
                 div(class = "filter-block",
                     icon("user", class = "fa-2x", style = "color:white;"),
                     div("Joueur", class = "filter-label"),
                     shinyWidgets::checkboxGroupButtons(
                       inputId = "joueurs_analyse",
                       label = NULL,
                       choices = c("Cristiano Ronaldo", "Lionel Messi"),
                       selected = c("Cristiano Ronaldo", "Lionel Messi"),
                       status = "primary",
                       justified = TRUE,
                       checkIcon = list(
                         yes = icon("check-circle", style = "color:#e84393"),
                         no = icon("circle", style = "color:#003366")
                       ),
                       individual = TRUE,
                       size = "lg"
                     ),
                     
                     uiOutput("badge_joueurs")
                 ),
                 div(class = "filter-block",
                     icon("trophy", class = "fa-2x", style = "color:white;"),
                     div("Compétition", class = "filter-label"),
                     uiOutput("select_competition"),
                     uiOutput("badge_competition")
                 ),
                 div(class = "filter-block",
                     icon("calendar-alt", class = "fa-2x", style = "color:white;"),
                     div("Saison", class = "filter-label"),
                     uiOutput("select_saison"),
                     uiOutput("badge_saison")
                 )
             ),
             # Résumé dynamique
             uiOutput("resume_dynamique"),
             # KPI en cartes
             div(class = "kpi-row",
                 div(class = "kpi-card",
                     icon("futbol", class = "fa-2x", style = "color:#e84393;"),
                     div("Buts totaux", class = "kpi-title"),
                     bsTooltip("kpi_buts", "Nombre total de buts selon la sélection.", "right", options = list(container = "body")),
                     tags$span(textOutput("kpi_buts", inline = TRUE), id = "kpi_buts", class = "kpi-value"),
                     icon("question-circle", class = "kpi-help", id = "help_buts")
                 ),
                 div(class = "kpi-card",
                     icon("building", class = "fa-2x", style = "color:#003366;"),
                     div("Clubs différents", class = "kpi-title"),
                     bsTooltip("kpi_clubs", "Nombre de clubs différents pour lesquels les buts ont été marqués.", "right", options = list(container = "body")),
                     tags$span(textOutput("kpi_clubs", inline = TRUE), id = "kpi_clubs", class = "kpi-value"),
                     icon("question-circle", class = "kpi-help", id = "help_clubs")
                 ),
                 div(class = "kpi-card",
                     icon("trophy", class = "fa-2x", style = "color:#e84393;"),
                     div("Compétitions", class = "kpi-title"),
                     bsTooltip("kpi_competitions", "Nombre de compétitions différentes selon la sélection.", "right", options = list(container = "body")),
                     tags$span(textOutput("kpi_competitions", inline = TRUE), id = "kpi_competitions", class = "kpi-value"),
                     icon("question-circle", class = "kpi-help", id = "help_competitions")
                 ),
                 div(class = "kpi-card",
                     icon("bullseye", class = "fa-2x", style = "color:#003366;"),
                     div("Types de but", class = "kpi-title"),
                     bsTooltip("kpi_types", "Nombre de types de buts différents (pied, tête, coup franc, etc.).", "right", options = list(container = "body")),
                     tags$span(textOutput("kpi_types", inline = TRUE), id = "kpi_types", class = "kpi-value"),
                     icon("question-circle", class = "kpi-help", id = "help_types")
                 )
             ),
             # Navigation verticale
             fluidRow(
               column(
                 width = 3,
                 navlistPanel(
                   well = FALSE,
                   tabPanel(title = tagList(icon("chart-line"), "Évolution"), value = "evolution"),
                   tabPanel(title = tagList(icon("futbol"), "Types de buts"), value = "types"),
                   tabPanel(title = tagList(icon("hands-helping"), "Passeurs"), value = "passeurs"),
                   tabPanel(title = tagList(icon("shield-alt"), "Adversaires"), value = "adversaires"),
                   tabPanel(title = tagList(icon("table"), "Tableau"), value = "tableau"),
                   id = "sousonglets_analyse"
                 )
               ),
               column(
                 width = 9,
                 uiOutput("analyse_content")
               )
             )
    ),
    
    tabPanel("GOAT🐐",
             tabPanel(
               HTML('GOAT <span style="font-size:1.3em; vertical-align:middle;">🐐</span>'),
               tags$div(
                 style = "max-width:700px; margin:auto; background:#f7f7f7; padding:38px 30px 30px 30px; margin-top:30px; box-shadow:0 1px 4px #ccc; border-radius:0; text-align:center;",
                 h2("QUI EST LE GOAT SELON VOUS ?"),
                 p("Votez pour le joueur qui incarne, à vos yeux, la légende ultime du football mondial."),
                 tags$div(
                   id = "goat-choices",
                   style = "display:flex; justify-content:center; gap:42px; margin:38px 0 28px 0;",
                   # CR7
                   actionButton(
                     inputId = "vote_cr7",
                     label = HTML(
                       '<div style="display:flex;flex-direction:column;align-items:center;">
            <img src="Cristiano Ronaldo.jpg" height="120" style="border-radius:12px;box-shadow:0 2px 12px #00336644; margin-bottom:12px; border:3px solid #e84393;">
            <span style="font-family:\'Oswald\',sans-serif;font-size:1.25em;color:#003366;letter-spacing:1px;">CRISTIANO RONALDO</span>
          </div>'
                     ),
                     style = "background:#fff; border:3px solid #e84393; border-radius:18px; box-shadow:0 2px 8px #eee; width:180px; height:210px; padding:0; font-weight:bold; font-size:1.1em; transition:box-shadow 0.2s,border 0.2s;",
                     class = "goat-btn"
                   ),
                   # Messi
                   actionButton(
                     inputId = "vote_messi",
                     label = HTML(
                       '<div style="display:flex;flex-direction:column;align-items:center;">
            <img src="Lionel Messi.jpg" height="120" style="border-radius:12px;box-shadow:0 2px 12px #00336644; margin-bottom:12px; border:3px solid #e84393;">
            <span style="font-family:\'Oswald\',sans-serif;font-size:1.25em;color:#003366;letter-spacing:1px;">LIONEL MESSI</span>
          </div>'
                     ),
                     style = "background:#fff; border:3px solid #e84393; border-radius:18px; box-shadow:0 2px 8px #eee; width:180px; height:210px; padding:0; font-weight:bold; font-size:1.1em; transition:box-shadow 0.2s,border 0.2s;",
                     class = "goat-btn"
                   )
                 ),
                 uiOutput("resultat_goat"),
                 tags$div(
                   style = "margin-top:30px; color:#003366; font-size:1.1em;",
                   "Ce vote est anonyme et ne modifie pas les statistiques de l’application."
                 ),
                 tags$style(HTML("
      .goat-btn:hover, .goat-btn.selected {
        border:3px solid #003366 !important;
        box-shadow:0 4px 18px #e8439340;
        background:#e84393 !important;
        color:#fff !important;
      }
      .goat-btn.selected span { color:#e84393 !important; }
    "))
               )
             )
             
    ),
    tabPanel("À PROPOS",
             tags$div(
               style = "max-width:700px; margin:auto; background:#f7f7f7; padding:30px; margin-top:30px; box-shadow:0 1px 4px #ccc; border-radius:0;",
               h2("À propos de l’application"),
               p("Cette application a été créée pour tous les passionnés de football qui souhaitent comparer, explorer et débattre autour des performances de deux légendes : Lionel Messi et Cristiano Ronaldo."),
               p("Grâce à une approche 100% data, chaque but, chaque club, chaque compétition et chaque adversaire sont passés au crible pour offrir une vision objective et interactive de leur rivalité."),
               tags$blockquote(
                 style = "font-style:italic; color:#e84393; border-left:4px solid #e84393; padding-left:16px; background:rgba(232,67,147,0.07);",
                 HTML("« La data ne remplace pas l’émotion, mais elle éclaire le mythe. »")
               ),
               h4("Sources et méthodologie"),
               tags$ul(
                 tags$li("Données issues de Kaggle(buts officiels en clubs et en séléction"),
                 tags$li("Analyses et visualisations réalisées avec R et Shiny"),
                 tags$li("Tous les chiffres sont mis à jour et vérifiés régulièrement")
               ),
               h4("Pourquoi la data foot ?"),
               p("La data est devenue incontournable dans le football moderne : clubs, entraîneurs, médias et fans l’utilisent pour mieux comprendre les performances, ajuster les tactiques et nourrir les débats. Cette application s’inscrit dans cette tendance, en rendant l’analyse accessible à tous."),
               h4("Auteur"),
               p("Développé par EMMANUEL DJEDJE &  KOUASSI GUY CHARLES EMMANUEL, passionnés de data et de football."),
      
               p("Pour toute suggestion, question ou correction : emmanueldjedje57@gmail.com.")
             )
             
    ),
    tabPanel("CONTACT",
             tags$div(
               style = "max-width:700px; margin:auto; background:#f7f7f7; padding:40px 30px; margin-top:30px; box-shadow:0 1px 8px #ccc; border-radius:0; font-family:'Montserrat',sans-serif; color:#003366;",
               h2(style = "font-family:'Oswald',sans-serif; font-weight:bold; color:#e84393; margin-bottom:30px;", "Contactez-nous"),
               p("Vous avez une question, une suggestion ou souhaitez collaborer ? Nous sommes à votre écoute !"),
               tags$div(
                 style = "display:flex; flex-direction: column; gap:20px; font-size:1.1em;",
                 tags$div(
                   style = "display:flex; align-items:center; gap:15px;",
                   icon("envelope", class = "fa-lg", style = "color:#e84393;"),
                   tags$a(href = "mailto:emmanueldjedje57@gmail.com", "emmanueldjedje57@gmail.com", style = "color:#003366; text-decoration:none;"),
                   
                 ),
                 tags$div(
                   style = "display:flex; align-items:center; gap:15px;",
                   icon("phone", class = "fa-lg", style = "color:#e84393;"),
                   tags$span("+225 07 57 22 33 40")
                 ),
                 tags$div(
                   style = "display:flex; align-items:center; gap:15px;",
                   icon("linkedin", class = "fa-lg", style = "color:#e84393;"),
                   tags$a(href = "www.linkedin.com/in/emmanuel-levy-djedje-1b2339315", "Emmanuel Levy DJEDJE", target = "_blank", style = "color:#003366; text-decoration:none;")
                 )
               ),
               tags$hr(style = "margin:30px 0; border-color:#e84393;"),
               p("Merci de votre intérêt pour cette application. Votre retour est précieux pour l’améliorer continuellement."),
               tags$div(
                 style = "text-align:center; margin-top:30px;",
                 actionButton("send_message", "Envoyer un message", 
               
                                             style = "background:#e84393; color:white; font-size:18px; border-radius:0; padding:12px 40px; font-family:'Oswald',sans-serif; font-weight:bold;")
                  ),
               tags$script(HTML("
  Shiny.addCustomMessageHandler('openMail', function(email) {
    window.location.href = 'mailto:' + email;
  });
"))
               
             )
    )
    
    
    
    
  )
)
