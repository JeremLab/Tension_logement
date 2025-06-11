library(shiny)
library(sf)
library(ggrepel)
library(bslib)
library(readxl)
library(leaflet)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)

options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
tmpfile59 <- tempfile(fileext = ".geojson")
download.file(
  "https://france-geojson.gregoiredavid.fr/repo/departements/59-nord/arrondissements-59-nord.geojson",
  destfile = tmpfile59, mode = "wb"
)
dep59 <- st_read(tmpfile59)

tmpfile62 <- tempfile(fileext = ".geojson")
download.file(
  "https://france-geojson.gregoiredavid.fr/repo/departements/62-pas-de-calais/arrondissements-62-pas-de-calais.geojson",
  destfile = tmpfile62, mode = "wb"
)
dep62 <- st_read(tmpfile62)

npdc <- rbind(dep59, dep62)

####### Indicateurs colonnes ----------
tooltips <- c(
  "Année universitaire étudiée",
  "Secteur géographique",
  "Nombre total de logements",
  "Nombre de places \ndisponibles au tour logement",
  "Nombre de places dédiées \nà la phase complémentaire\n    (places non réservés)",
  "% des places non réservés \npar rapport aux places disponibles",
  "Nombre d'étudiants distinct ayant \nfait une demande dans ce secteur",
  "Demandes enregistrées lors du dernier tour",
  "Nombre moyen de demandes par étudiant",
  "Représentation du secteur dans l'ensemble des demandes (%)",
  "Calcul : demandes/places disponibles",
  "Tension ajustée à l'effet ParcourSup\nCalcul : \n`Demandes uniques dernier tour` +
(`Place disponible` - `Places phase complémentaire`)/`Place disponible`",
  "Tension minimale observée lors du dernier tour",
  "Nombre de renouvellements confirmés",
  "Part des logements renouvelés"
)

# Interface utilisateur ------
ui <- fluidPage(
  # Titre visible avec image
  tags$head(
    tags$script(HTML("
      window.localStorage.clear();
      window.sessionStorage.clear();
    "))
  ),
  div(
    style = "display: flex; align-items: center; padding: 10px; border-bottom: 1px solid #ccc;",
    tags$img(
      src = "https://irtshdf.fr/wp-content/uploads/2021/04/Crous-logo-lille-nord-pas-de-calais.png",
      height = "60px",
      style = "margin-right: 10px;"
    ),
    tags$h1("Tension des logements étudiants")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Télécharger un fichier Excel", accept = c(".xls", ".xlsx")),
      actionButton("select", "Sélectionner / Désélectionner tout"),
      br(), br(),
      checkboxGroupInput("bassin",
        label = "Choix bassin :",
        choices = list(
          "Arras" = "arras",
          "Béthune" = "Béthune",
          "Boulogne-sur-Mer" = "Boulogne-sur-Mer",
          "Calais" = "Calais",
          "Cambrai" = "cambrai",
          "Lens-Liévin" = "lens-liévin",
          "Lille Centre" = "Lille Centre",
          "Lille Est" = "Lille Est",
          "Longuenesse - Saint Omer" = "Longuenesse - Saint Omer",
          "Maubeuge" = "maubeuge",
          "Roubaix - Tourcoing" = "roubaix - tourcoing",
          "Valenciennes" = "valenciennes"
        ),
        selected = c(
          "arras", "Béthune", "Boulogne-sur-Mer", "Calais", "cambrai", "lens-liévin",
          "Lille Centre", "Lille Est", "Longuenesse - Saint Omer", "maubeuge",
          "roubaix - tourcoing", "valenciennes"
        )
      ),
      uiOutput("select_annee_ui")
    ),
    mainPanel(
      navset_card_underline(
        nav_panel(
          "Secteur",
          plotOutput("NPDC", width = "100%", height = "500px"),
          div(
            style = "display: flex; justify-content: flex-end;",
            actionButton("btn1", "Brut"),
            actionButton("btn2", "Réel")
          ),
          br(), br(),
          DTOutput("preview")
        ),
        nav_panel(
          "Résidence",
          plotlyOutput("bubbleChart1", width = "100%", height = "400px"),
          DTOutput("résidence")
        ),
        nav_panel(
          "Global",
          plotlyOutput("graph2", width = "100%", height = "400px"),
          DTOutput("global")
        )
      )
    )
  )
)

# Serveur ------
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    on.exit(unlink(input$file$datapath), add = TRUE)
    read_excel(input$file$datapath)
  })

  all_choices <- c(
    "arras", "Béthune", "Boulogne-sur-Mer", "Calais", "cambrai",
    "lens-liévin", "Lille Centre", "Lille Est", "Longuenesse - Saint Omer",
    "maubeuge", "roubaix - tourcoing", "valenciennes"
  )
  observeEvent(input$select, {
    if (setequal(input$bassin, all_choices)) {
      updateCheckboxGroupInput(session, "bassin", selected = character(0))
      updateActionButton(session, "toggle_all", label = "Tout sélectionner")
    } else {
      updateCheckboxGroupInput(session, "bassin", selected = all_choices)
      updateActionButton(session, "toggle_all", label = "Désélectionner tout")
    }
  })

  output$select_annee_ui <- renderUI({
    req(data())
    choices <- sort(unique(data()[["Année de gestion"]]))
    selectInput(
      "select_annee",
      label = h3("Année de gestion"),
      choices = choices,
      selected = min(choices, na.rm = TRUE)
    )
  })

  graph_choice <- reactiveVal("brut")

  observeEvent(input$btn1, {
    graph_choice("brut")
  })

  observeEvent(input$btn2, {
    graph_choice("réel")
  })

  #----------- Graphique --------------
  output$NPDC <- renderPlot(
    {
      req(data())

      # Calculs par secteur (comme pour le tableau)
      demandes <- data() %>%
        filter(!is.na(`Secteur`), !is.na(`INE`)) %>%
        distinct(`Année de gestion`, `Secteur`, `INE`) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(Demandes = n(), .groups = "drop") %>%
        group_by(`Année de gestion`) %>%
        mutate(poids_secteur = round(Demandes / sum(Demandes), 4)) %>%
        ungroup()

      demandes_tour4 <- data() %>%
        filter(!is.na(INE), !is.na(`Sous-phase (Libellé)`)) %>%
        mutate(
          num_tour = as.numeric(gsub("\\D", "", `Sous-phase (Libellé)`))
        ) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(INE, `Année de gestion`, `Secteur`) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(`Demandes uniques dernier tour` = n(), .groups = "drop")

      places_total_résidence <- data() %>%
        filter(
          !is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`),
          !is.na(`Année de gestion`)
        ) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, `Nombre logement`) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

      places_par_residence <- data() %>%
        filter(
          !is.na(`Secteur`), !is.na(`Places Total`), !is.na(`Résidence`),
          !is.na(`Année de gestion`)
        ) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places Total`) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(
          `Place disponible` = sum(`Places Total`, na.rm = TRUE),
          .groups = "drop"
        )
      coords_secteur <- data() %>%
        filter(!is.na(`Secteur`), !is.na(`Latitude`), !is.na(`Longitude`), !is.na(`Année de gestion`)) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(
          Latitude = mean(Latitude, na.rm = TRUE),
          Longitude = mean(Longitude, na.rm = TRUE),
          .groups = "drop"
        )

      #------------------ Phase complémentaire ----------------

      places_complémentaire <- data() %>%
        filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
        distinct(`Année de gestion`, `Secteur`, `Places phase complémentaire`) %>%
        group_by(`Année de gestion`, `Secteur`) %>%
        summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`), .groups = "drop")

      #------------------ Tension ----------------
      tension <- demandes %>%
        left_join(places_total_résidence, by = c("Année de gestion", "Secteur")) %>%
        left_join(places_par_residence, by = c("Année de gestion", "Secteur")) %>%
        left_join(demandes_tour4, by = c("Année de gestion", "Secteur")) %>%
        left_join(coords_secteur, by = c("Année de gestion", "Secteur")) %>%
        left_join(places_complémentaire, by = c("Année de gestion", "Secteur")) %>%
        mutate(
          `Tension brut` = round(Demandes / `Place disponible`, 2),
          moyenne_poids = mean(poids_secteur, na.rm = TRUE),
          `Concentration secteur` = round(poids_secteur / moyenne_poids, 2),
          `Tension pondérée` = round((`Demandes uniques dernier tour` +
            (`Place disponible` - `Places phase complémentaire`))
          / `Place disponible`, 2),
        )
      # Application des filtres graphiques
      map_data <- tension %>%
        filter(
          tolower(Secteur) %in% tolower(input$bassin),
          `Année de gestion` == input$select_annee
        )

      if (nrow(map_data) == 0) {
        plot.new()
        title(main = "Aucune donnée à afficher")
      } else {
        tension_col <- if (graph_choice() == "brut") "Tension brut" else "Tension pondérée"
        map_data$label <- paste0(map_data$Secteur, "\n", map_data[[tension_col]])

        map_data$tension_classe <- cut(
          map_data[[tension_col]],
          breaks = c(-Inf, 1, 5, 8, 10, Inf),
          labels = c("≤1", "1 à 4", "5 à 7", "8 à 9", ">10"),
          right = FALSE
        )
        map_data$tension_classe <- factor(
          map_data$tension_classe,
          levels = c(">10", "8 à 9", "5 à 7", "1 à 4", "≤1")
        )

        ggplot() +
          geom_sf(data = npdc, fill = NA, color = "#004080", size = 0.4) +
          geom_point(
            data = map_data,
            aes(
              x = Longitude,
              y = Latitude,
              size = tension_classe,
              fill = tension_classe
            ),
            color = "white",
            stroke = 0.5,
            shape = 21
          ) +
          geom_label_repel(
            data = map_data,
            aes(x = Longitude, y = Latitude, label = label),
            color = "white",
            fill = alpha("black", 0.6),
            size = 3.5,
            fontface = "bold",
            box.padding = 0.6,
            point.padding = 0.7,
            segment.color = "grey30",
            segment.size = 0.6,
            min.segment.length = 0
          ) +
          scale_fill_manual(
            values = c(
              "≤1" = "#A8D38D",
              "1 à 4" = "#7BA05B",
              "5 à 7" = "#FFD990",
              "8 à 9" = "#FFA970",
              ">10" = "#BC2023"
            ),
            labels = c(
              "≤1" = "≤1",
              "1 à 4" = "1 à 4",
              "5 à 7" = "5 à 7",
              "8 à 9" = "8 à 9",
              ">10" = ">10"
            ),
            name = paste("Tension", if (graph_choice() == "brut") "brute" else "réelle")
          ) +
          scale_size_manual(
            values = c(
              "≤1" = 2,
              "1 à 4" = 4,
              "5 à 7" = 8,
              "8 à 9" = 12,
              ">10" = 16
            ),
            labels = c(
              "≤1" = "≤1",
              "1 à 4" = "1 à 4",
              "5 à 7" = "5 à 7",
              "8 à 9" = "8 à 9",
              ">10" = ">10"
            ),
            name = paste("Tension", if (graph_choice() == "brut") "brute" else "réelle")
          ) +
          guides(
            fill = guide_legend(override.aes = list(shape = 21, color = "white")),
            size = guide_legend(override.aes = list(shape = 21, color = "white"))
          ) +
          ggtitle(
            paste0(
              "Taux de tension ",
              if (graph_choice() == "brut") "brut" else "réel",
              " logement ",
              input$select_annee
            )
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 11.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.margin = grid::unit(rep(0, 4), "lines"),
          )
      }
    },
    res = 80
  )

  output$bubbleChart1 <- renderPlotly({
    req(data())
    places_par_residence1 <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Places Total`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places Total`)

    tension_data1 <- places_par_residence1 %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(
        Places1 = sum(`Places Total`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        data() %>%
          filter(!is.na(`Secteur`), !is.na(`INE`), !is.na(`Résidence`)) %>%
          distinct(`Année de gestion`, `Secteur`, `Résidence`, `INE`) %>%
          group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
          summarise(
            Demandes1 = n(),
            .groups = "drop"
          ),
        by = c("Année de gestion", "Secteur", "Résidence")
      ) %>%
      mutate(
        Tension1 = round(Demandes1 / Places1, 2)
      ) %>%
      filter(
        tolower(Secteur) %in% tolower(input$bassin),
        `Année de gestion` == input$select_annee
      )
    # Filtrer les 10 résidences avec le plus de tension
    top10 <- tension_data1 %>%
      group_by(`Année de gestion`) %>%
      mutate(Résidence = factor(Résidence, levels = sort(unique(Résidence)))) %>%
      arrange(desc(Tension1)) %>%
      slice_head(n = 10) %>%
      ungroup()

    p1 <- ggplot(top10, aes(
      x = `Demandes1`,
      y = Tension1,
      size = Tension1,
      color = Résidence,
      text = paste(
        "Année :", `Année de gestion`,
        "<br>Résidence :", `Résidence`,
        "<br>Places:", Places1,
        "<br>Demandes:", Demandes1,
        "<br>Tension:", Tension1
      )
    )) +
      geom_point(alpha = 0.6, position = position_jitter(width = 0.25, height = 0)) +
      scale_size(range = c(2, 10)) +
      scale_y_continuous(trans = "log10") +
      labs(
        x = "Nombre de demande",
        y = "Taux de tension",
        color = "Résidence",
        size = NULL,
        title = "Taux de tension des 10 résidences les plus demandées par année"
      ) +
      theme_minimal()

    ggplotly(p1, tooltip = "text")
  })
  output$graph2 <- renderPlotly({
    req(data())

    filtered_data <- data() %>%
      filter(!is.na(`Secteur`)) %>%
      filter(tolower(`Secteur`) %in% tolower(input$bassin))

    # Places
    places_total_résidence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Nombre logement`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

    place_data <- filtered_data %>%
      filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Places disponibles` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")

    # Demandes
    demandes <- filtered_data %>%
      filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      distinct(`Année de gestion`, `Secteur`, `INE`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Demandes uniques` = n(), .groups = "drop") %>%
      group_by(`Année de gestion`) %>%
      mutate(poids_secteur = round(`Demandes uniques` / sum(`Demandes uniques`), 2)) %>%
      ungroup()

    demandes_tour4 <- data() %>%
      filter(!is.na(INE), !is.na(`Sous-phase (Libellé)`)) %>%
      mutate(
        num_tour = as.numeric(gsub("\\D", "", `Sous-phase (Libellé)`))
      ) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(INE, `Année de gestion`, `Secteur`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Demandes uniques dernier tour` = n(), .groups = "drop") %>%
      group_by(`Année de gestion`) %>%
      summarise(`Demandes uniques dernier tour` = sum(`Demandes uniques dernier tour`), .groups = "drop")

    # Phase complémentaire
    places_complémentaire <- filtered_data %>%
      filter(!is.na(`Secteur`), !is.na(`Places phase complémentaire`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`) %>%
      summarise(
        `Places phase complémentaire` =
          sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop"
      )

    # Résumé année globale avec la formule FINALE de tension pondérée
    resume_annee <- place_data %>%
      group_by(`Année de gestion`) %>%
      summarise(
        `Places disponibles` = sum(`Places disponibles`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        demandes %>%
          group_by(`Année de gestion`) %>%
          summarise(`Demandes uniques` = sum(`Demandes uniques`), .groups = "drop"),
        by = "Année de gestion"
      ) %>%
      left_join(places_complémentaire, by = "Année de gestion") %>%
      left_join(demandes_tour4, by = "Année de gestion") %>%
      mutate(
        `Tension pondérée` = round((`Demandes uniques dernier tour` + (`Places disponibles` -
          `Places phase complémentaire`)) / `Places disponibles`, 2)
      ) %>%
      arrange(`Année de gestion`)

    # Graphique avec plotly
    plot_ly(resume_annee, x = ~ factor(`Année de gestion`)) %>%
      add_bars(
        y = ~`Demandes uniques`, name = "Demandes uniques",
        marker = list(color = "#F4A280"),
        text = ~`Demandes uniques`, textposition = "outside",
        hoverinfo = "none"
      ) %>%
      add_bars(
        y = ~`Demandes uniques dernier tour`, name = "Demandes dernier tour",
        marker = list(color = "#F4A261"),
        text = ~`Demandes uniques dernier tour`, textposition = "outside",
        hoverinfo = "none"
      ) %>%
      add_bars(
        y = ~`Places disponibles`, name = "Places disponibles",
        marker = list(color = "#7EB3D5"),
        text = ~`Places disponibles`, textposition = "outside",
        hoverinfo = "none"
      ) %>%
      add_bars(
        y = ~`Places phase complémentaire`, name = "Places phase complémentaire",
        marker = list(color = "#7EB1E7"),
        text = ~`Places phase complémentaire`, textposition = "outside",
        hoverinfo = "none"
      ) %>%
      add_lines(
        y = ~`Tension pondérée`, name = "Tension pondérée", yaxis = "y2",
        line = list(color = "#3498db", width = 3),
        mode = "lines+markers",
        marker = list(color = "#3498db", size = 8),
        text = ~ paste("Tension pondérée :", `Tension pondérée`),
        hoverinfo = "text"
      ) %>%
      layout(
        barmode = "group",
        title = list(
          text = "Évolution des demandes, places et tension pondérée par année",
          font = list(size = 18, family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = "Année de gestion", tickangle = -45,
          titlefont = list(size = 14), tickfont = list(size = 12)
        ),
        yaxis = list(
          title = "Demandes et places", titlefont = list(size = 14),
          tickfont = list(size = 12),
          showgrid = TRUE, gridcolor = "rgba(200,200,200,0.3)"
        ),
        yaxis2 = list(
          title = "Tension pondérée",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          title_standoff = 70,
          automargin = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center"
        )
      )
  })

  #----------- Tableau Interractif -----------
  output$preview <- renderDT({
    req(data()) 

    #------------------ Demandes ----------------

    demandes <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`INE`)) %>%
      distinct(`Année de gestion`, `Secteur`, `INE`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Demandes uniques` = n(), .groups = "drop") %>%
      group_by(`Année de gestion`) %>%
      mutate(poids_secteur = `Demandes uniques` / sum(`Demandes uniques`)) %>%
      ungroup()

    demandes_tour4 <- data() %>%
      filter(!is.na(INE), !is.na(`Sous-phase (Libellé)`)) %>%
      mutate(
        num_tour = as.numeric(gsub("\\D", "", `Sous-phase (Libellé)`))
      ) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(INE, `Année de gestion`, `Secteur`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Demandes uniques dernier tour` = n(), .groups = "drop")

    demandes_moyennes <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`INE`)) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(
        total_voeux = n(),
        nb_demandeurs = n_distinct(INE),
        `Demande moyenne` = round(total_voeux / nb_demandeurs, 0),
        .groups = "drop"
      )

    #------------------ Places ----------------

    places_total_résidence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Nombre logement`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

    places_par_residence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Places Total`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places Total`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(
        `Place disponible` = sum(`Places Total`, na.rm = TRUE),
        .groups = "drop"
      )

    #------------------ Phase complémentaire ----------------

    places_complémentaire <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`), .groups = "drop")

    #------------------ Tension ----------------

    tension <- demandes %>%
      left_join(places_total_résidence, by = c("Année de gestion", "Secteur")) %>%
      left_join(places_par_residence, by = c("Année de gestion", "Secteur")) %>%
      left_join(demandes_tour4, by = c("Année de gestion", "Secteur")) %>%
      mutate(
        `Tension brute` = round(`Demandes uniques` / `Place disponible`, 2),
        moyenne_poids = mean(poids_secteur, na.rm = TRUE),
        `Concentration secteur` = round(poids_secteur / moyenne_poids, 2),
        `Concentration secteur (%)` = round(poids_secteur * 100, 2),
        `Tension minimale` = round(`Demandes uniques dernier tour` / `Place disponible`, 2)
      )

    #------------------ Renouvellement ----------------

    renouvellement_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")

    renouvellement_data1 <- renouvellement_data %>%
      left_join(places_total_résidence, by = c("Année de gestion", "Secteur")) %>%
      mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))

    #------------------ Filtres ----------------

    filter_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Places Total`), !is.na(`INE`)) %>%
      filter(tolower(`Secteur`) %in% tolower(input$bassin)) %>%
      filter(`Année de gestion` == input$select_annee) %>%
      group_by(`Année de gestion`, `Secteur`, `INE`) %>%
      summarise(Total_Voeux = n(), .groups = "drop") %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(
        Total_Distinct_INE = n_distinct(`INE`),
        .groups = "drop"
      ) %>%
      left_join(tension, by = c("Année de gestion", "Secteur")) %>%
      left_join(places_total_résidence, by = c("Année de gestion", "Secteur")) %>%
      left_join(demandes_moyennes, by = c("Année de gestion", "Secteur")) %>%
      left_join(renouvellement_data1, by = c("Année de gestion", "Secteur")) %>%
      left_join(places_complémentaire, by = c("Année de gestion", "Secteur"))

    final <- filter_data %>%
      mutate(
        `(%) Non réservé` = round(`Places phase complémentaire` / `Place disponible` * 100, 2),
        `Tension pondérée` = round((`Demandes uniques dernier tour` + (`Place disponible` -
          `Places phase complémentaire`)) / `Place disponible`, 2),
      ) %>%
      select(
        `Année de gestion`, `Secteur`, `Logement total`, `Place disponible`,
        `Places phase complémentaire`, `(%) Non réservé`, `Demandes uniques`, `Demandes uniques dernier tour`,
        `Demande moyenne`,
        `Concentration secteur (%)`, `Tension brute`, `Tension pondérée`, `Tension minimale`,
        `Renouvellement confirmé`, `(%) du parc`
      )

    datatable(
      final,
      colnames = c(
        "Année de gestion",
        "Secteur",
        "Logement total",
        "Place disponible",
        "Places phase complémentaire",
        "(%) Non réservé",
        "Demandes uniques",
        "Demandes uniques dernier tour",
        "Demande moyenne",
        "Concentration secteur (%)",
        "Tension brute",
        "Tension pondérée",
        "Tension minimale",
        "Renouvellement confirmé",
        "(%) du parc"
      ),
      escape = FALSE,
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        initComplete = JS(
          sprintf(
            "function(settings, json) {
          var tips = %s;
          var header = this.api().table().header();
          for (var i = 0; i < tips.length; i++) {
            header.children[0].children[i].setAttribute('title', tips[i]);
          }
        }",
            jsonlite::toJSON(tooltips)
          )
        )
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })
  output$résidence <- renderDT({
    req(data()) 

    #------------------ Places ----------------
    places_total_résidence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Résidence`, `Nombre logement`, `Secteur`) %>%
      group_by(`Année de gestion`, `Résidence`, `Secteur`) %>%
      summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

    places_par_residence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Places Total`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places Total`)

    places_data <- places_par_residence %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(
        `Places disponibles` = sum(`Places Total`, na.rm = TRUE),
        .groups = "drop"
      )
    #------------------ Demandes ----------------
    demandes_par_residence <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`INE`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `INE`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(
        `Demandes uniques` = n_distinct(`INE`),
        .groups = "drop"
      )

    demandes_tour4 <- data() %>%
      filter(!is.na(INE), `Sous-phase (Libellé)` == "Tour 4") %>%
      distinct(INE, `Année de gestion`, `Secteur`, `Résidence`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(`Demandes uniques dernier tour` = n(), .groups = "drop")

    #------------------ Phase complémentaire  ----------------

    places_complémentaire <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(`Places phase complémentaire` = first(`Places phase complémentaire`), .groups = "drop")

    #------------------ Renouvellement ----------------

    renouvellement_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
      group_by(`Année de gestion`, `Résidence`, `Secteur`) %>%
      summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")

    renouvellement_data1 <- renouvellement_data %>%
      left_join(places_total_résidence, by = c("Année de gestion", "Résidence", "Secteur")) %>%
      mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))

    #------------------ Tension ----------------
    tension_data <- places_data %>%
      left_join(demandes_par_residence, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(demandes_tour4, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(renouvellement_data1, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(places_complémentaire, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      mutate(
        Tension = round(`Demandes uniques` / `Places disponibles`, 2),
        `Tension minimal` = round(`Demandes uniques dernier tour` / `Places disponibles`, 2)
      )

    final <- tension_data %>%
      filter(tolower(`Secteur`) %in% tolower(input$bassin)) %>%
      filter(`Année de gestion` == input$select_annee) %>%
      mutate(
        `Non réservé (%)` = round(`Places phase complémentaire` / `Places disponibles` * 100, 2)
      ) %>%
      select(
        `Année de gestion`, `Secteur`, `Résidence`, `Places disponibles`, `Places phase complémentaire`,
        `Non réservé (%)`, `Demandes uniques`, `Demandes uniques dernier tour`, Tension,
        `Tension minimal`, `Renouvellement confirmé`,
        `(%) du parc`
      )


    datatable(
      final,
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        initComplete = JS(
          sprintf(
            "function(settings, json) {
          var tips = %s;
          var header = this.api().table().header();
          for (var i = 0; i < tips.length; i++) {
            header.children[0].children[i].setAttribute('title', tips[i]);
          }
        }",
            jsonlite::toJSON(tooltips)
          )
        )
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })
  output$résidence <- renderDT({
    req(data()) # Vérifie que les données existent

    #------------------ Places ----------------
    places_total_résidence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Résidence`, `Nombre logement`, `Secteur`) %>%
      group_by(`Année de gestion`, `Résidence`, `Secteur`) %>%
      summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

    places_par_residence <- data() %>%
      filter(
        !is.na(`Secteur`), !is.na(`Places Total`), !is.na(`Résidence`),
        !is.na(`Année de gestion`)
      ) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places Total`)

    places_data <- places_par_residence %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(
        `Places disponibles` = sum(`Places Total`, na.rm = TRUE),
        .groups = "drop"
      )
    #------------------ Demandes ----------------
    demandes_par_residence <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`INE`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `INE`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(
        `Demandes uniques` = n_distinct(`INE`),
        .groups = "drop"
      )

    demandes_tour4 <- data() %>%
      filter(!is.na(INE), `Sous-phase (Libellé)` == "Tour 4") %>%
      distinct(INE, `Année de gestion`, `Secteur`, `Résidence`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(`Demandes uniques dernier tour` = n(), .groups = "drop")

    #------------------ Phase complémentaire  ----------------

    places_complémentaire <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(`Places phase complémentaire` = first(`Places phase complémentaire`), .groups = "drop")

    #------------------ Renouvellement ----------------

    renouvellement_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
      group_by(`Année de gestion`, `Résidence`, `Secteur`) %>%
      summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")

    renouvellement_data1 <- renouvellement_data %>%
      left_join(places_total_résidence, by = c("Année de gestion", "Résidence", "Secteur")) %>%
      mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))

    #------------------ Tension ----------------
    tension_data <- places_data %>%
      left_join(demandes_par_residence, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(demandes_tour4, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(renouvellement_data1, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      left_join(places_complémentaire, by = c("Année de gestion", "Secteur", "Résidence")) %>%
      mutate(
        Tension = round(`Demandes uniques` / `Places disponibles`, 2),
        `Tension minimal` = round(`Demandes uniques dernier tour` / `Places disponibles`, 2)
      )

    final <- tension_data %>%
      filter(tolower(`Secteur`) %in% tolower(input$bassin)) %>%
      filter(`Année de gestion` == input$select_annee) %>%
      mutate(
        `Non réservé (%)` = round(`Places phase complémentaire` / `Places disponibles` * 100, 2)
      ) %>%
      select(
        `Année de gestion`, `Secteur`, `Résidence`, `Places disponibles`, `Places phase complémentaire`,
        `Non réservé (%)`, `Demandes uniques`, `Demandes uniques dernier tour`, Tension,
        `Tension minimal`, `Renouvellement confirmé`,
        `(%) du parc`
      )


    datatable(
      final,
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })
  output$global <- renderDT({
    req(data())

    #---- Données filtrées par bassin ----
    filtered_data <- data() %>%
      filter(!is.na(`Secteur`)) %>%
      filter(tolower(`Secteur`) %in% tolower(input$bassin))

    #------------------ PLACES ----------------
    places_total_résidence <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Nombre logement`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Logement total` = sum(`Nombre logement`, na.rm = TRUE), .groups = "drop")

    place_data <- filtered_data %>%
      filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places disponibles` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")

    #------------------ DEMANDES ----------------
    demandes <- filtered_data %>%
      filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      distinct(`Année de gestion`, `Secteur`, `INE`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Demandes uniques` = n(), .groups = "drop")

    #------------------ DEMANDES DERNIER TOUR ----------------
    demandes_tour4 <- data() %>%
      filter(!is.na(INE), !is.na(`Sous-phase (Libellé)`)) %>%
      mutate(num_tour = as.numeric(gsub("\\D", "", `Sous-phase (Libellé)`))) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(INE, `Année de gestion`, `Secteur`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Demandes uniques dernier tour` = n(), .groups = "drop")

    #------------------ PHASE COMPLÉMENTAIRE ----------------
    places_complémentaire <- filtered_data %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Résidence`) %>%
      summarise(`Places phase complémentaire` = first(`Places phase complémentaire`), .groups = "drop") %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop")

    #------------------ RENOUVELLEMENT ----------------
    renouvellement_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")

    renouvellement_data1 <- renouvellement_data %>%
      left_join(places_total_résidence, by = "Année de gestion") %>%
      mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))

    #------------------ Final ----------------
    resume_annee <- place_data %>%
      left_join(places_total_résidence, by = "Année de gestion") %>%
      left_join(demandes, by = "Année de gestion") %>%
      left_join(demandes_tour4, by = "Année de gestion") %>%
      left_join(places_complémentaire, by = "Année de gestion") %>%
      left_join(renouvellement_data1[, c("Année de gestion", "Renouvellement confirmé", "(%) du parc")], by = "Année de gestion") %>%
      arrange(`Année de gestion`) %>%
      mutate(
        `Tension brute` = round(`Demandes uniques` / `Places disponibles`, 2),
        `Tension pondérée` = round((`Demandes uniques dernier tour` + (`Places disponibles` - `Places phase complémentaire`)) / `Places disponibles`, 2),
        `Évolution demandes (%)` = round((`Demandes uniques` / lag(`Demandes uniques`) - 1) * 100, 1),
        `Évolution places (%)` = round((`Places disponibles` / lag(`Places disponibles`) - 1) * 100, 1),
        `(%) Non réservé` = round(`Places phase complémentaire` / `Places disponibles` * 100, 2)
      )

    final1 <- resume_annee %>%
      select(
        `Année de gestion`, `Logement total`, `Places disponibles`, `Places phase complémentaire`,
        `(%) Non réservé`, `Demandes uniques`, `Demandes uniques dernier tour`, `Tension brute`,
        `Tension pondérée`, `Évolution places (%)`, `Évolution demandes (%)`, `Renouvellement confirmé`,
        `(%) du parc`
      )

    datatable(
      final1,
      options = list(
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)