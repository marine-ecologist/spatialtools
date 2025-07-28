library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(grDevices)
library(cxhull)

species_pal <- setNames(
  c("#3A9AB2", "#ADC397", "#DFBF2B", "#EA8005", "#72B2BF",
    "#EE5A03", "#95BBB1", "#E5A208", "#CAC96A", "#F11B00"),
  paste0("Species_", 1:10)
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Multiple Species Niche Simulator"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css")
  ),
  tags$style(HTML("
    #view2d, #view3d {
      height: 42px;
      display: flex;
      align-items: center;
    }
    #view2d {
      background-color: #9e9e9e;
      color: white;
      font-weight: bold;
      margin-right: 10px;
      margin-bottom: 10px;
    }
    #view3d {
      background-color: #616161;
      color: white;
      font-weight: bold;
      margin-right: 10px;
      margin-bottom: 10px;
    }
    .nutrient-sliders {
      display: none;
    }
    .irs-min, .irs-max {
      font-size: 80% !important;
    }
    .irs-shadow, .irs-single, .irs-grid-text, .irs-from, .irs-to {
      display: none !important;
    }
    .irs {
      border: none !important;
    }
  ")),

  fluidRow(
    column(12,
           div(style = "display: flex; align-items: center;",
               actionButton("view2d", tagList(span(class = "fa-regular fa-square", style = "font-size:20px; margin-right: 8px;"), "2D niche space")),
               actionButton("view3d", tagList(span(class = "fa-solid fa-cube", style = "font-size:20px; margin-right: 8px;"), "3D niche hypervolume"))
           )
    )
  ),
  div(style = "background-color: #ffffff; padding: 0px; border-radius: 5px; margin: 20px;",
      numericInput("n_species", "Number of Species", min = 1, max = 10, value = 2)
  ),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: white;",
      uiOutput("species_tabs")
    ),
    mainPanel(
      plotlyOutput("scatterPlot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  view_mode <- reactiveVal("2D")

  observeEvent(input$view2d, {
    view_mode("2D")
    shinyjs::runjs("document.querySelectorAll('.nutrient-sliders').forEach(e => e.style.display = 'none');")
  })

  observeEvent(input$view3d, {
    view_mode("3D")
    shinyjs::runjs("document.querySelectorAll('.nutrient-sliders').forEach(e => e.style.display = 'block');")
  })

  output$species_tabs <- renderUI({
    species_names <- c("coral", "seagrass", paste0("Species_", 3:10))

    insertUI(selector = "head", where = "beforeEnd", ui = tags$style(HTML(
      paste0(
        lapply(1:input$n_species, function(i) {
          sprintf(
            "#species_panels li:nth-child(%d) a { background-color: %s !important; color: white !important; }",
            i, species_pal[[i]]
          )
        }),
        collapse = "\n"
      )
    )))

    tabs <- lapply(1:input$n_species, function(i) {
      tabPanel(
        title = uiOutput(paste0("tab_title_", i)),
        style = paste0("background-color: rgba(", paste(col2rgb(species_pal[[i]]), collapse = ","), ",0.1); color: black; padding: 10px;"),
        value = paste0("Species_", i),
        fluidRow(
          div(style = "display: flex; align-items: center; gap: 20px; margin: 10px 20px;",
              div(style = "flex: 2;",
                  textInput(paste0("species_name_", i), "Species Name", value = species_names[i])
              ),
              div(style = "flex: 0.6;",
                  numericInput(paste0("n_", i), "n individuals", min = 5, max = 200, value = 150, step = 2)
              )
          )
        ),
        div(class = "well-section",
            div(style = "display: flex; gap: 50px;",
                div(style = "flex: 1;",
                    sliderInput(
                      paste0("mean_temp_", i),
                      tagList(span(class = "fa fa-thermometer-half", style = "margin-right: 6px;"), strong("Temperature (°C)")),
                      min = 16, max = 34, value = c(30, 25, rep(20, 8))[i], step = 1
                    )
                ),
                div(style = "flex: 0.5;",
                    sliderInput(paste0("sd_temp_", i), "", min = 0, max = 3, value = c(0.2, 3, rep(1, 8))[i], step = 0.1)
                )
            )
        ),
        div(class = "well-section",
            div(style = "display: flex; gap: 50px;",
                div(style = "flex: 1;",
                    sliderInput(
                      paste0("mean_light_", i),
                      tagList(span(class = "fa fa-sun", style = "margin-right: 6px;"), strong("Light (PAR)")),
                      min = 100, max = 1000, value = c(800, 400, rep(500, 8))[i], step = 100
                    )
                ),
                div(style = "flex: 0.5;",
                    sliderInput(paste0("sd_light_", i), "", min = 1, max = 200, value = c(40, 60, rep(25, 8))[i], step = 10)
                )
            )
        ),
        div(class = "well-section nutrient-sliders",
            div(style = "display: flex; gap: 50px;",
                div(style = "flex: 1;",
                    sliderInput(
                      paste0("mean_nutrients_", i),
                      tagList(span(class = "fa fa-droplet", style = "margin-right: 6px;"), strong("Nutrients (μmol)")),
                      min = 0, max = 50, value = c(10, 45, rep(25, 8))[i], step = 1
                    )
                ),
                div(style = "flex: 0.5;",
                    sliderInput(paste0("sd_nutrients_", i), "", min = 0, max = 5, value = c(0.5, 4, rep(2, 8))[i], step = 0.25)
                )
            )
        )
      )
    })

    lapply(1:input$n_species, function(i) {
      output[[paste0("tab_title_", i)]] <- renderUI({
        req(input[[paste0("species_name_", i)]])
        span(input[[paste0("species_name_", i)]])
      })
    })

    tagList(
      checkboxGroupInput("display_options", label = NULL,
                         choices = c("Show points", "Show hulls"),
                         selected = c("Show points", "Show hulls"),
                         inline = TRUE),
      do.call(tabsetPanel, c(id = "species_panels", tabs))
    )
  })

  output$scatterPlot <- renderPlotly({
    req(input$n_species)
    species_list <- list()
    mesh_list <- list()

    safe_rnorm <- function(n, mean, sd, min_val, max_val) {
      req(mean, sd, min_val, max_val)
      vals <- c()
      while (length(vals) < n) {
        new_vals <- rnorm(n, mean = mean, sd = sd)
        new_vals <- new_vals[new_vals >= min_val & new_vals <= max_val]
        vals <- c(vals, new_vals)
      }
      vals[1:n]
    }

    for (i in seq_len(input$n_species)) {
      n <- input[[paste0("n_", i)]]
      mean_temp <- input[[paste0("mean_temp_", i)]]
      sd_temp <- input[[paste0("sd_temp_", i)]]
      mean_light <- input[[paste0("mean_light_", i)]]
      sd_light <- input[[paste0("sd_light_", i)]]
      species_name <- input[[paste0("species_name_", i)]]

      temp <- safe_rnorm(n, mean = mean_temp, sd = sd_temp, min_val = 15, max_val = 35)
      light <- safe_rnorm(n, mean = mean_light, sd = sd_light, min_val = 10, max_val = 1000)

      df <- data.frame(Temperature = temp, Light = light, Species = species_name)

      if (view_mode() == "3D") {
        mean_nutrients <- input[[paste0("mean_nutrients_", i)]]
        sd_nutrients <- input[[paste0("sd_nutrients_", i)]]
        df$Nutrients <- safe_rnorm(n, mean = mean_nutrients, sd = sd_nutrients, min_val = 0, max_val = 50)

        mat <- as.matrix(df[, c("Temperature", "Light", "Nutrients")])
        if (nrow(mat) >= 4) {
          hull <- cxhull(mat)
          mesh <- hullMesh(hull)
          vertices <- mesh$vertices
          faces <- mesh$faces
          mesh_list[[species_name]] <- list(vertices = vertices, faces = faces)
        }
      }

      species_list[[i]] <- df
    }

    full_df <- bind_rows(species_list)
    p <- plot_ly()

    if (view_mode() == "2D") {
      hulls <- full_df |>
        group_by(Species) |>
        slice(chull(Temperature, Light)) |>
        ungroup()

      for (sp in unique(full_df$Species)) {
        df <- filter(full_df, Species == sp)
        df_hull <- filter(hulls, Species == sp)
        col <- species_pal[[which(unique(full_df$Species) == sp)]]

        if ("Show hulls" %in% input$display_options) {
          p <- p |>
            add_trace(
              data = df_hull,
              type = 'scatter', mode = 'lines',
              x = c(df_hull$Temperature, df_hull$Temperature[1]),
              y = c(df_hull$Light, df_hull$Light[1]),
              fill = 'toself',
              fillcolor = adjustcolor(col, alpha.f = 0.3),
              line = list(color = 'black', width = 2),
              name = paste(sp, "hull"),
              showlegend = FALSE, hoverinfo = 'skip'
            )
        }

        if ("Show points" %in% input$display_options) {
          p <- p |>
            add_trace(
              data = df,
              x = ~Temperature, y = ~Light,
              type = 'scatter', mode = 'markers',
              marker = list(color = col, size = 5),
              name = sp
            )
        }
      }

      p <- p |>
        layout(
          xaxis = list(title = "Temperature (°C)", range = c(15, 35)),
          yaxis = list(title = "Light (PAR)", range = c(10, 1000))
        )

    } else {
      for (sp in unique(full_df$Species)) {
        df <- filter(full_df, Species == sp)
        col <- species_pal[[which(unique(full_df$Species) == sp)]]

        if ("Show points" %in% input$display_options) {
          p <- p |>
            add_trace(
              data = df,
              x = ~Nutrients, y = ~Temperature, z = ~Light,  # x and y swapped
              type = 'scatter3d', mode = 'markers',
              marker = list(color = col, size = 4),
              name = sp
            )
        }

        if ("Show hulls" %in% input$display_options && !is.null(mesh_list[[sp]])) {
          v <- mesh_list[[sp]]$vertices
          f <- mesh_list[[sp]]$faces

          p <- p |>
            add_trace(
              x = v[, 3], y = v[, 1], z = v[, 2],  # x=Nutrients, y=Temperature, z=Light
              i = f[, 1] - 1, j = f[, 2] - 1, k = f[, 3] - 1,
              type = "mesh3d",
              opacity = 0.3,
              facecolor = rep(list(col), nrow(f)),
              name = paste(sp, "hull"),
              showlegend = FALSE
            )
        }
      }

      p <- p |>
        layout(
          scene = list(
            xaxis = list(title = "Nutrients (μmol)", range = c(0, 50)),
            yaxis = list(title = "Temperature (°C)", range = c(15, 35)),
            zaxis = list(title = "Light (PAR)", range = c(10, 1000))
          )
        )


    }

    p
  })

}

shinyApp(ui, server)


