create_card <- function(id, label) {
    div(
      id = id,
      strong(label),
      br(),
      br(),
      shinyWidgets::radioGroupButtons(
        inputId = paste0("check_", id),
        label = NULL,
        choices = c("Yes", "No"),
        justified = TRUE,
        individual = TRUE,
        selected = character(0)
      ),
      textAreaInput(
        inputId = paste0("item_", id, "_comment"),
        label = NULL,
        placeholder = "Comment",
        width = "1500px",
        height = "50px"
      )
  )
}

  card_labels <- list(
  "1" = "Data files are accessible and in an open repository *[(click here)](https://journals.plos.org/plosone/s/recommended-repositories)*",
  "2" = "Data are associated with a license *[(click here)](https://creativecommons.org/chooser/)*",
  "3" = "Data files are present and complete",
  "4" = "Data files are in an interoperable format",
  "5" = "Data metadata present and adequate",
  
  "6" = "The structure and contents of the archived data files must match the description in the manuscript.",
  
  "7" = "Code files are accessible and in an open repository (e.g., Zenodo or figshare)",
  "8" = "Code is associated with a licence *[(click here)](choosealicense.com)*",
  "9" = "Code files are present and complete",
  "10" = "Code is in an interoperable format",
  "11" = "Code metadata present and adequate",
  
  "12" = "The structure and content of the archived code must match the description of data filtering, processing, and analysis, and the presentation of results in the manuscript.",
  
  "13" = "Code must be able to run without error using the archived data. With the exception of easy to fix file path errors, all errors should be addressed by the author",

  "14" = "Numeric results (in table or text)",
  "15" = "Figures"
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