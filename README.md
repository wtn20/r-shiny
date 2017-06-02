# r-shiny
A collection of R Shiny aps

# Installation

To run an app on a Mac, install R
`>brew tap homebrew/science`
`>brew install r`
   
   
Navigate to the folder containing the app (i.e. the folder containing the ui.R and server.R files)

Open an R console
`>R`

Import the shiny library
`library(shiny)`

Note: If you do not have Shiny installed, first download it
`install.packages("shiny")`

To run the app type:
`runApp("app-name")`
where `app-name` is the name of the folder

Alternatively, if you have R-Studio installed, you can open either the ui.R or the server.R files, 
and click the 'Run App' button in the top right corner of the file window.
