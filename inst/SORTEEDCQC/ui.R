
ui <- function() {
 
  bslib::page_fluid(
    theme = bslib::bs_theme(
      version = 5,
      base_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
      code_font = bslib::font_collection(bslib::font_google("Atkinson Hyperlegible"), "Arial Narrow Bold", "sans-serif"),
     bootswatch = "zephyr"
    ),
    
    shinyjs::useShinyjs(),
    title = "SORTEE DCQC",
    fluidRow(
      column(
        width = 1,
        actionButton(
          inputId = "clickme",
          label = tags$img(src = "circle_black.png", height = "78px", width = "78px",
                        onclick ="window.open('https://www.sortee.org/', '_blank')"),
          style = "color: white; background-color: white; border-color: white; box-shadow: 0px 0px 0px 0px white;",
        )
      ),
      column(
        width = 11,
        div(
          style = "display: flex; align-items: center; height: 78px;",
          h1("SORTEE Data and Code Quality Control", style = "margin: 0;")
        )
      )
    ),
    bslib::layout_sidebar(
      sidebar =
        bslib::sidebar(
          width = 400,
          position = "left",
          open = "open",
          shiny::div(
            id = "review_summary",
            bslib::card(
            shiny::htmlOutput("paper_title_output"),
            shiny::htmlOutput("reviewer_name_output"),
            shiny::htmlOutput("journal_name_output")
          )
          ),
          shiny::downloadButton("download_DCQCpdf", "Download Text Report",class = "btn-info")
        ),
      div(id = "stage1_title",
      h4("Stage 1: Data must be archived and adhere to FAIR guiding principles.")
      ),
      create_card("1", "Data files are accessible and in an open repository"),
      br(),
      create_card("2", "Data are associated with a license"),
      br(),
      create_card("3", "Data files are present and complete"),
      br(),
      create_card("4", "Data files are in an interoperable format"),
      br(),
      create_card("5", "Data metadata present and adequate"),
      br(),
      div(id = "comments_1",
          h4("Addtional comments")
      ),
      create_card_comment("1a", "Comments"),
      br(),
      br(),
      div(id = "stage2_title",
      h4("Stage 2: Archived data corresponds with the data reported in the manuscript")
      ),
      create_card("6", "The structure and contents of the archived data files must match the description in the manuscript"),
      br(),
      div(id = "comments_2",
          h4("Addtional comments")
      ),
      create_card_comment("2a", "Comments"),
      br(),
      br(),
      div(id = "stage3_title",
      h4("Stage 3: Code must be archived and adhere to FAIR guiding principles")
      ),
      create_card("7", "Code files are accessible and in an open repository"),
      br(),
      create_card("8", "Code is associated with a licence"),
      br(),
      create_card("9", "Code files are present and complete"),
      br(),
      create_card("10", "Code is in an interoperable format"),
      br(),
      create_card("11", "Code metadata present and adequate"),
      br(),
      div(id = "comments_3",
          h4("Addtional comments")
      ),
      create_card_comment("3a", "Comments"),
      br(),
      br(),
      div(id = "stage4_title",
      h4("Stage 4: Archived code corresponds with the workflow reported in the manuscript")
      ),
      create_card("12", "The structure and content of the archived code must match the description of data filtering, processing, and analysis, and the presentation of results in the manuscript."),
      br(),
      div(id = "comments_4",
          h4("Addtional comments")
      ),
      create_card_comment("4a", "Comments"),
      br(),
      br(),
      div(id = "stage5_title",
      h4("Stage 5: Archived code runs with the archived data")
      ),
      create_card("13", "Code must be able to run without error using the archived data. With the exception of easy to fix file path errors, all errors should be addressed by the author."),
      br(),
      div(id = "comments_5",
          h4("Addtional comments")
      ),
      create_card_comment("5a", "Comments"),
      br(),
      br(),
      div(id = "stage6_title",
      h4("Stage 6: Results can be computationally reproduced by running the archived code")
      ),
      create_card("14", "Numeric results (in table or text)"),
      br(),
      create_card("15", "Figures"),
      br(),
      div(id = "comments_6",
          h4("Addtional comments")
      ),
      create_card_comment("6a", "Comments"),
      br(),
      br(),
    )
    )
}
