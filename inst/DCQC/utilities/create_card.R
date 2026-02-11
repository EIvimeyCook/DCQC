create_card <- function(id, label) {
  label_html <- gsub("\\*\\[(.+?)\\]\\((.+?)\\)\\*", '<a href="\\2" target="_blank">\\1</a>', label)
  
  div(
    id = id,
    HTML(paste0("<strong>", label_html, "</strong>")),
    br(),
    br(),
    shinyWidgets::radioGroupButtons(
      inputId = paste0("check_", id),
      label = NULL,
      choices = c("Yes", "No"),
      justified = TRUE,
      width = "220px",
      individual = TRUE,
      selected = character(0),
      size = "normal"
    ),
    textAreaInput(
      inputId = paste0("item_", id, "_comment"),
      label = NULL,
      placeholder = "Comment",
      width = "1000px",
      height = "50px"
    )
  )
}

card_labels <- list(
  
  "1" = "Data files are accessible and in an open repository [(click here)](https://journals.plos.org/plosone/s/recommended-repositories). Data must be placed in an open repository with a permanent Digital Object Identifier that is cited in the manuscript. Data must not be in the supplementary material, as it does not provide a DOI.",
  
  "2" = "Data are associated with a license [(click here)](https://creativecommons.org/chooser/). The license must be clearly visible in the repository in order to permit reuse. Legally, no license means that data cannot be legally reused under many circumstances.",
  
  "3" = "Data files are present and complete. All files required to reproduce the results described in the manuscript must be included. Ideally, raw data alongside processed data and code should be archived. If this is not possible, a sample of raw data or a detailed description of how to obtain and process the raw data is required.",
  
  "4" = "Data files are in an interoperable format. Files should be provided in non-proprietary, widely used formats (e.g. CSV, TSV, TXT) or include an open alternative if proprietary formats are used.",
  
  "5" = "Data metadata present and adequate. Metadata (e.g. in a README.txt) must describe file contents, variable names, units, alongside manuscript information, author details, and any other relevant information, including the data license and any funding requirements.",
  
  "6" = "The structure and contents of the archived data files must match the description in the manuscript. For instance, sample sizes and variable names must be consistent with the relevant manuscript sections.",
  
  "7" = "Code files are accessible and in an open repository (e.g., Zenodo or figshare). The repository must be publicly accessible and provide a stable link or identifier that is cited in the manuscript.",
  
  "8" = "Code is associated with a licence [(click here)](https://choosealicense.com). The licence must be explicitly stated and permit reuse, modification, and redistribution of the code.",
  
  "9" = "Code files are present and complete. All scripts required for data processing, analyses, and outputs must be included.",
  
  "10" = "Code is in an interoperable format. Code should be written in widely used, open languages (e.g. R, Python) and avoid proprietary code where possible. Code must not be archived in a .pdf or .doc format.",
  
  "11" = "Code metadata present and adequate. A detailed README.txt must be present and explain the purpose of each script, execution order, inputs, outputs, and required packages or software versions.",
  
  "12" = "The structure and content of the archived code must match the description of data filtering, processing, and analysis, and the presentation of results in the manuscript. Analytical steps described in the manuscript must be identifiable in the code.",
  
  "13" = "Code must be able to run without error using the archived data. With the exception of easy-to-fix file path errors, all errors should be addressed by the author. Successful execution should reproduce the reported analyses without manual intervention.",
  
  "14" = "Numeric results (in table or text). Reported numerical values (e.g. sample sizes, estimates, test statistics, confidence intervals, p-values) must be reproducible from the archived code and data. It should be noted if a degree of tolerance is allowed by the data editor (e.g. using Hardwicke et al. (2021): (reproduced − original) / original × 100).",
  
  "15" = "Figures. All figures must be reproducible from the archived code and data. It should be noted if a degree of tolerance is allowed by the data editor."
  
)


  create_card_comment <- function(id, label) {
    div(
      id = id,
      label = NULL,
      textAreaInput(
        inputId = paste0("item_", id, "_comment"),
        label = NULL,
        placeholder = "Comment",
        width = "1500px",
        height = "50px"
      )
    )
  }