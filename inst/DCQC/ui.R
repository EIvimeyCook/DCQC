
ui <- function() {
 
  bslib::page_fluid(
    theme = bslib::bs_theme(
      version = 5,
     bootswatch = "zephyr"
    ),
    shiny::tags$script(shiny::HTML("
  Shiny.addCustomMessageHandler('openHTML', function(html) {
    var win = window.open('', '_blank');
    win.document.write(html);
    win.document.close();
  });
")),
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
          shiny::actionButton("download_DCQCmd", "View HTML Report", class = "btn-info")
        ),
      div(id = "stage1_title",
      h4("Stage 1: Data must be archived and adhere to FAIR guiding principles.")
      ),
      create_card("1", "Data files are accessible and in an open repository. Data files are accessible and in an open repository (https://journals.plos.org/plosone/s/recommended-repositories). Data must be placed in an open repository with a permanent Digital Object Identifier that is cited in the manuscript. Data must not be in the supplementary material, as it does not provide a DOI."),
      br(),
      create_card("2", "Data are associated with a license (https://creativecommons.org/chooser/). The license must be clearly visible in the repository in order to permit reuse. Legally, no license means that data cannot be legally reused under many circumstances"),
      br(),
      create_card("3", "Data files are present and complete. All files required to reproduce the results described in the manuscript must be included. Ideally, raw data alongside processed data and code should be archived. If this is not possible, a sample of raw data or a detailed description of how to obtain and process the raw data is required."),
      br(),
      create_card("4", "Data files are in an interoperable format. Files should be provided in non-proprietary, widely used formats (e.g. CSV, TSV, TXT) or include an open alternative if proprietary formats are used."),
      br(),
      create_card("5", "Data metadata present and adequate. Metadata (e.g. in a README.txt) must describe file contents, variable names, units, alongside manuscript information, author details, and any other relevant information, including the data license and any funding requirements."),
      br(),
      div(id = "stage2_title",
      h4("Stage 2: Archived data corresponds with the data reported in the manuscript")
      ),
      create_card("6", "The structure and contents of the archived data files must match the description in the manuscript. For instance, sample sizes and variable names must be consistent with the relevant manuscript sections."),
      br(),
      div(id = "stage3_title",
      h4("Stage 3: Code must be archived and adhere to FAIR guiding principles")
      ),
      create_card("7", "Code files are accessible and in an open repository (e.g., Zenodo or figshare). The repository must be publicly accessible and provide a stable link or identifier that is cited in the manuscript."),
      br(),
      create_card("8", "Code is associated with a licence (https://choosealicense.com). The licence must be explicitly stated and permit reuse, modification, and redistribution of the code."),
      br(),
      create_card("9", "Code files are present and complete. All scripts required for data processing, analyses, and outputs must be included."),
      br(),
      create_card("10", "Code is in an interoperable format. Code should be written in widely used, open languages (e.g. R, Python) and avoid proprietary code where possible. Code must not be archived in a .pdf or .doc format."),
      br(),
      create_card("11", "Code metadata present and adequate. A detailed README.txt must be present and explain the purpose of each script, execution order, inputs, outputs, and required packages or software versions."),
      br(),
      div(id = "stage4_title",
      h4("Stage 4: Archived code corresponds with the workflow reported in the manuscript")
      ),
      create_card("12", "The structure and content of the archived code must match the description of data filtering, processing, and analysis, and the presentation of results in the manuscript. Analytical steps described in the manuscript must be identifiable in the code."),
      br(),
      div(id = "stage5_title",
      h4("Stage 5: Archived code runs with the archived data")
      ),
      create_card("13", "Code must be able to run without error using the archived data. With the exception of easy-to-fix file path errors, all errors should be addressed by the author. Successful execution should reproduce the reported analyses without manual intervention."),
      br(),
      div(id = "stage6_title",
      h4("Stage 6: Results can be computationally reproduced by running the archived code")
      ),
      create_card("14", "Numeric results (in table or text). Reported numerical values (e.g. sample sizes, estimates, test statistics, confidence intervals, p-values) must be reproducible from the archived code and data. It should be noted if a degree of tolerance is allowed by the data editor (e.g. using Hardwicke et al. (2021): (reproduced − original) / original × 100)."),
      br(),
      create_card("15", "Figures. All figures must be reproducible from the archived code and data. It should be noted if a degree of tolerance is allowed by the data editor."),
      br()
    )
    )
}
