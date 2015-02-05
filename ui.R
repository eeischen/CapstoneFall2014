library(shiny)
shinyUI(pageWithSidebar(
  headerPanel(
    h1("Word Prediction App", align="center"),
    windowTitle="Word Prediction App"
    ),
  sidebarPanel(
    p("This app predicts the next word in a phrase.  If you enter a phrase (containing at least one word) in the text box to the right, 
      it will provide a prediction for the next word in the phrase.")
  ),
  mainPanel(
     textInput(inputId="phrasebegin", label = "Type a phrase (containing at least one word) here:"),
    textOutput('lastword'),
     textOutput('phraseend')
   )
))