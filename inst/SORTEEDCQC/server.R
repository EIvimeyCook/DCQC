server <- function(input, output, session) {
  
  output$download_DCQCpdf <- shiny::downloadHandler(
    filename = function() {
      paste0("DCQC_Report_", input$paper_title, "_", input$reviewer_name, ".rtf")
    },
    content = function(file) {
      template_path <- system.file("rmd", "DCQCreport.Rmd", package = "shinyDCQC")
      tempReport <- file.path(tempdir(), "DCQCreport.Rmd")
      file.copy(template_path, tempReport, overwrite = TRUE)

      card_data <- lapply(names(card_labels), function(id) {
        response <- input[[paste0("check_", id)]]
        comment <- input[[paste0("item_", id, "_comment")]]
        if (!is.null(response)) {
          list(
            id = id,
            label = card_labels[[id]],
            response = response,
            comment = comment
          )
        } else {
          NULL
        }
      })

      card_data <- Filter(Negate(is.null), card_data)

      paper_title <- if (input$paper_title == "") "No Title Given" else input$paper_title
      reviewer_name <- if (input$reviewer_name == "") "No Name Given" else input$reviewer_name
      journal_name <- if (input$journal_name == "") "No Journal Given" else input$journal_name
      
      params <- list(
        paper_title = paper_title,
        reviewer_name = reviewer_name,
        journal_name = journal_name,
        report_date = Sys.Date(),
        card_data = card_data
      )

      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )



  data_modal <- shiny::modalDialog(
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE,
    shiny::div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;",
      shiny::tags$img(src = "circle_black.png", height = "88px", width = "88px", style = "margin-bottom: 20px;"),
      br(),
      textInput("paper_title", tags$b("Paper Title"), placeholder = "Enter paper name", width = "300px"),
      textInput("reviewer_name", tags$b("Reviewer Name"), placeholder = "Enter your name", width = "300px"),
      textInput("journal_name", tags$b("Journal"), placeholder = "Enter journal name", width = "300px"),
      br(),
      br(),
      shinyWidgets::awesomeCheckboxGroup(
        inputId = "stage_checks",
        label = tags$b("Select DCQC Review Stages"),
        choices = paste("Stage", 1:6),
        selected = character(0),
        inline = TRUE,
        status = "success"
      ),
      shiny::htmlOutput("Stage_guides"),
      actionButton("submit_data_modal", "Submit", class = "btn btn-success", style = "margin-top: 20px;")
    )
  )
  shinyjs::hide("stage1_title")
  shinyjs::hide("stage2_title")
  shinyjs::hide("stage3_title")
  shinyjs::hide("stage4_title")
  shinyjs::hide("stage5_title")
  shinyjs::hide("stage6_title")
  shinyjs::hide("comments_1")
  shinyjs::hide("comments_2")
  shinyjs::hide("comments_3")
  shinyjs::hide("comments_4")
  shinyjs::hide("comments_5")
  shinyjs::hide("comments_6")

  shinyjs::hide("1")
  shinyjs::hide("2")
  shinyjs::hide("3")
  shinyjs::hide("4")
  shinyjs::hide("5")
  shinyjs::hide("6")
  shinyjs::hide("7")
  shinyjs::hide("8")
  shinyjs::hide("9")
  shinyjs::hide("10")
  shinyjs::hide("11")
  shinyjs::hide("12")
  shinyjs::hide("13")
  shinyjs::hide("14")
  shinyjs::hide("15")
  shinyjs::hide("1a")
  shinyjs::hide("2a")
  shinyjs::hide("3a")
  shinyjs::hide("4a")
  shinyjs::hide("5a")
  shinyjs::hide("6a")
  shinyjs::hide("review_summary")
  shinyjs::hide("download_DCQCpdf")
  shiny::showModal(data_modal)

  shiny::observeEvent(input$submit_data_modal, {
    
    output$paper_title_output <- shiny::renderUI({
      if(input$paper_title == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Title:</b> No Title Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Title:</b>",
        input$paper_title,
        "</p>"
      ))
      )
    })

    output$reviewer_name_output <- shiny::renderUI({
      if(input$reviewer_name == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Name:</b> No Name Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Name:</b>",
        input$reviewer_name,
        "</p>"
      ))
      )
    })

    output$journal_name_output <- shiny::renderUI({
      if(input$journal_name == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Journal:</b> No Journal Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Journal:</b>",
        input$journal_name,
        "</p>"
      ))
      )
    })

    if ("Stage 1" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage1_title")
      shinyjs::show("1")
      shinyjs::show("2")
      shinyjs::show("3")
      shinyjs::show("4")
      shinyjs::show("5")
      shinyjs::show("comments_1")
      shinyjs::show("1a")
    }

    if ("Stage 2" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage2_title")
      shinyjs::show("6")
      shinyjs::show("comments_2")
      shinyjs::show("2a")
    }

    if ("Stage 3" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage3_title")
      shinyjs::show("7")
      shinyjs::show("8")
      shinyjs::show("9")
      shinyjs::show("10")
      shinyjs::show("11")
      shinyjs::show("comments_3")
      shinyjs::show("3a")
    }

    if ("Stage 4" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage4_title")
      shinyjs::show("12")
      shinyjs::show("comments_4")
      shinyjs::show("4a")
    }

    if ("Stage 5" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage5_title")
      shinyjs::show("13")
      shinyjs::show("comments_5")
      shinyjs::show("5a")
    }

    if ("Stage 6" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCpdf")
      shinyjs::show("stage6_title")
      shinyjs::show("14")
      shinyjs::show("15")
      shinyjs::show("comments_6")
      shinyjs::show("6a")
    }

    if (length(input$stage_checks) == 0) {
      shinyalert::shinyalert(
        title = "Select a stage to review",
        text = "No stage selected",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })

  shiny::observe({
    if (all(c("Stage 1", "Stage 2") %in% input$stage_checks) &&
      !any(c("Stage 3", "Stage 4", "Stage 5", "Stage 6") %in% input$stage_checks)) {
      output$Stage_guides <- renderUI({
        HTML("<div style='font-size: 22px; font-weight: bold; margin-top: 10px;'>Data Re-use</div>")
      })
    } else if (all(c("Stage 1", "Stage 2", "Stage 3", "Stage 4") %in% input$stage_checks) &&
      !any(c("Stage 5", "Stage 6") %in% input$stage_checks)) {
      output$Stage_guides <- renderUI({
        HTML("<div style='font-size: 22px; font-weight: bold; margin-top: 10px;'>Data Reuse & Transparency</div>")
      })
    } else if (all(c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Stage 5", "Stage 6") %in% input$stage_checks)) {
      output$Stage_guides <- renderUI({
        HTML("<div style='font-size: 22px; font-weight: bold; margin-top: 10px;'>Data Reuse, Transparency, & Computational Reproducibility</div>")
      })
    } else {
      output$Stage_guides <- renderUI({
        NULL
      })
    }
  })
}
