library(shiny)
library(shinydashboard)
library(fmsb)
library(plotrix)
library(DT)
library(readxl)
library(stringr)
library(dplyr)

### Combining weighted and unweighted origami plots together ###
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Origami Plot App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("book")),
      menuItem("Origami Plot", tabName = "origami", icon = icon("chart-line")),
      menuItem("Weighted Origami Plot", tabName = "weighted_origami", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = shinythemes::shinytheme("cosmo")),
      tags$style(HTML("
        /* Body background color */
        body {
          background-color: #F8F8F8 !important;
        }
        .content-wrapper, .right-side {
          background-color: #F8F8F8 !important;
        }
        /* Increase font size for introduction tab */
        #introduction h2 {
          font-size: 24px !important;
        }
        #introduction p {
          font-size: 24px !important;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "introduction",
              h2("Introduction"),
              img(src = "https://github.com/Penncil/Origami_Plot_Generic/blob/master/origami_cover.png?raw=true", height = "619px", width = "1100px"),
              p("Radar charts are commonly used to depict multi-attribute data, but they can be misleading because their two-dimensional 
                areas do not invariably correspond to attribute values. In response, we introduce this Shiny web 
                app to implement origami plot proposed by Duan et al. in 2023 [1], which maintains the original functionality of a radar chart 
                and avoids potential misuse of its connected regions, with newly added features to better assist multi-criteria decision-making.
                The origami plot adds additional auxiliary axes and points such that area of the connected region of all dots is reflects each 
                attribute in relative proportions and is invariant to the ordering of those attributes. Origami Plot facilitates ranking 
                different objects by the overall performance and retains the intuitive visual appeal of radar charts. We demonstrate its 
                utility with example data, highlighting its effectiveness in supporting multi-criteria decision-making. "),
              p("[1] Duan, R., Tong, J., Sutton, A. J., Asch, D. A., Chu, H., Schmid, C. H., & Chen, Y. (2023). Origami plot: a novel 
                multivariate data visualization tool that improves radar chart. Journal of clinical epidemiology, 156, 85-94.")
      ),
      tabItem(tabName = "tutorial",
              h2("Tutorial"),
              img(src = "https://github.com/Penncil/Origami_Plot_Generic/blob/master/origami_figure_tutorial.png?raw=true", height = "1503px", width = "1000px"),
              p("A video version of the tutorial can be found at https://www.youtube.com/watch?v=YQtIftB9wyE&t=1s")
      ),
      tabItem(tabName = "origami",
              fluidRow(
                column(12,
                       title = "Upload Data",
                       downloadButton("downloadExample", "Download example csv file"),
                       downloadButton("downloadExampleXLSX", "Download example xlsx file"),
                       fileInput("file", "Choose CSV or XLSX File",
                                 multiple = FALSE,
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".xlsx")),
                       uiOutput("row_selector"),
                       # actionButton("submit", "Submit"),
                       checkboxInput("header", "Header", TRUE)
                ),
                box(title = "Input table", DTOutput('table')),
                #box(verbatimTextOutput("selected_rows")),
                box(
                  title = "Origami Plot",
                  actionButton("results_plot", "How to interpret origami plot?",
                               icon = icon("info-circle"),
                               style = "width: 200px; height: 20px; font-size: 12px; padding: 0px"),
                  uiOutput("origami_plots")
                )
              )
      ),
      tabItem(tabName = "weighted_origami",
              fluidRow(
                column(12,
                       title = "Upload Data",
                       downloadButton("weighted_downloadExample", "Download example data"),
                       downloadButton("weighted_downloadExampleXLSX", "Download example xlsx file"),
                       fileInput("weighted_file", "Choose CSV or XLSX File",
                                 multiple = FALSE,
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".xlsx")),
                       uiOutput("weighted_row_selector"),
                       textInput("weights", "Enter Weights (comma-separated)", value = NULL),
                       #actionButton("weighted_submit", "Submit"),
                       checkboxInput("weighted_header", "Header", TRUE)
                ),
                #column(6,title = "weight", verbatimTextOutput("weightsText")),
                box(title = "Input table", DTOutput('weighted_table')),
                box(
                  title = "Weighted Origami Plots",
                  actionButton("weighted_results_plot", "How to interpret origami plot?", 
                               icon = icon("info-circle"),
                               style = "width: 200px; height: 20px; font-size: 12px; padding: 0px"),
                  uiOutput("weighted_origami_plots")
                )
              )
      )
    )
  ),
  skin = "black"
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal()
  
  
  #################### For Unweighted Origami Plots ####################
  observeEvent(input$file, {
    req(input$file)
    if(stringr::str_ends(input$file$datapath, "csv")) {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (stringr::str_ends(input$file$datapath, "(xlsx|xls)")) {
      df <- readxl::read_excel(input$file$datapath) %>% as.data.frame()
    }
    data(df)
  })
  # Download example CSV
  output$downloadExample <- downloadHandler(
    filename = function() {
      "sucra.csv"
    },
    content = function(file) {
      file.copy("www/sucra.csv", file)
    }
  )
  
  output$downloadExampleXLSX <- downloadHandler(
    filename = function() {
      "sucra.xlsx"
    },
    content = function(file) {
      file.copy("www/sucra.xlsx", file)
    }
  )
  
  
  selected_data <- eventReactive(input$submit, {
    req(length(input$rows) == 1 | length(input$rows)==2)
    data()[as.numeric(input$rows), ]
  })
  # Print table for unweighted origami required data
  output$table <- renderDT({
    req(data())
    datatable(data(), selection = list(mode = "multiple", target = "row"))
  })
  
  #### =========== Original ===================
  # output$row_selector <- renderUI({
  #   req(data())
  #   checkboxGroupInput("rows", 
  #                      "Select 1 or 2 from the following indices:", 
  #                      choices = 1:nrow(data()), inline = TRUE)
  # })
  
  observeEvent(input$rows, {
    if(length(input$rows) > 2) {
      showModal(modalDialog(
        title = "Selection Error",
        "Please select 1 or 2 rows.",
        easyClose = TRUE,
        footer = NULL
      ))
      updateCheckboxGroupInput(session, "rows", selected = input$rows[1:2])
    }
  })
  
  #### =========== Modified by Jessie ===================
  output$row_selector <- renderUI({
    
    req(data())  # Ensure 'data' exists before rendering UI
    # Create checkbox input and an info button within the same UI context
    tagList(
      
      div(style = "display: flex; align-items: center;",
          checkboxGroupInput("rows",
                             "Select 1 or 2 from the following indices:",
                             choices = 1:nrow(data()), inline = TRUE),
          actionButton("infoBtn", "", icon = icon("info-circle"),
                       style = "width: 20px; height: 20px; 
                       font-size: 10px; padding: 5px;
                       background-color: #f8f8f8; 
                       color: #83a9e6; border: none;"),
          
      ),
      actionButton("submit", "Submit"),
    )
  })
  
  # Observe event for the 'More Info' button to show a modal dialog
  observeEvent(input$infoBtn, {
    showModal(modalDialog(
      title = "Additional instructions",
      HTML("Based on your preferences and needs, please select one of the following options:
      <ul class='custom-list'>
        <li><strong>Option 1:</strong> Select a single index from the list to create an origami plot for an individual object.</li>
        <li><strong>Option 2:</strong> Select exactly two indices from the list to generate a pairwise origami plot for comparing two objects.</li>
      </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # this is for results panel
  observeEvent(input$weighted_results_plot, {
    showModal(modalDialog(
      title = "Interpret the result",
      HTML("
      <ul class='custom-list'>
        <li><strong>Weighted Origami Plot:</strong> A weighted Origami plot visualizes the values of attributes for a single object. The attributes are represented as solid axes,
        with values marked along these axes. The weighted values are depicted with a red dashed line. A scale ranging from 0 to 1 appears below the output figure, where
        a green bar indicates the area of the unweighted object, and a red bar represents the area of the weighted object.</li>
      </ul>"),
      footer = modalButton("Close")
    ))
  })
  #===================== done ============================
  
  
  
  origami_plot_pairwise<- function(df, indices, min_value=NULL, pcol1=rgb(0.2,0.5,0.5,1), pfcol1=rgb(0.2,0.5,0.5,0.1),
                                   pcol2=rgb(0.6,0.3,0.3,1),pfcol2=rgb(0.6,0.3,0.3,0.1), axistype=1, seg=4, pty=16, plty=1:6, plwd=1,
                                   pdensity=NULL, pangle=45, cglty=1.4, cglwd=0.1,
                                   cglcol="#000000", axislabcol="#808080", title="",
                                   na.itp=TRUE, centerzero=TRUE, vlabels=NULL, vlcex=1,
                                   caxislabels=seq(0,1,by = 0.25), calcex=NULL,
                                   paxislabels=NULL, palcex=NULL) {
    row_names <- df[,1]
    df <- df[,2:ncol(df)]
    
    
    index1 = as.numeric(indices[1])
    
    
    if(length(indices)==1){
      index2 = index1
    } else {
      index2 = as.numeric(indices[2])
    }
    
    if(length(indices)==2) {title = paste0(row_names[index1]," vs. ", row_names[index2])}
    if(length(indices)==1) {title = row_names[index1]}
    
    df1 <- df[index1,]
    #check if object is valid
    if(dim(df1)[1]==0){ stop("The object is not present in the dataframe. Please ensure the object matches the row name exactly.")}
    if(dim(df1)[1]>1){ stop("The object is duplicated in the dataframe. Please ensure there are no duplicate row names.")}
    if(dim(df1)[1]==1){
      df1 <- data_preparation(df1,min_value = 0.15)
    }
    #min_value <- min(df1[3,])
    
    df2 <- df[index2,]
    #check if object is valid
    if(dim(df2)[1]==0){ stop("The object is not present in the dataframe. Please ensure the object matches the row name exactly.")}
    if(dim(df2)[1]>1){ stop("The object is duplicated in the dataframe. Please ensure there are no duplicate row names.")}
    if(dim(df2)[1]==1){
      df2 <- data_preparation(df2,min_value = 0.15)
    }
    
    df_list <- list(df1,df2)
    pcol_list <- list(pcol1,pcol2)
    pfcol_list <- list(pfcol1,pfcol2)
    num_figure = 2
    df <- df_list[[1]]
    n_prime <- ncol(df)/2
    aux_array_odd <- as.vector(seq(1,2*n_prime-1,2))
    aux_array_even <- as.vector(seq(2,2*n_prime,2))
    
    n_col = dim(df)[2]
    if (!is.data.frame(df)) { stop("The data must  be given as dataframe.\n"); return() }
    if ((n <- length(df))<3) { stop("The number of variables must be 3 or more.\n"); return() }
    plot(c(-1.2, 1.2), c(-1.2, 1.2), type="n", frame.plot=FALSE, axes=FALSE,
         xlab="", ylab="", main=title, asp=1) # define x-y coordinates without any plot
    theta <- seq(90, 450, length=n+1)*pi/180
    theta <- theta[1:n]
    xx <- cos(theta)
    yy <- sin(theta)
    CGap <- ifelse(centerzero, 0, 1)
    points(0,0, pch = 16, col  = rgb(0,0,0,0.2))
    for (ind in 1:n_col){
      if (ind == 1){
        segments(0, 0, 0, 1, lwd = 2, lty = 1, col = rgb(0,0,0,0.2)) # factor 1
      } else{
        if ((ind %% 2) == 0){
          draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 2, col = rgb(0,0,0,0.4))
        } else{
          draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 1, col = rgb(0,0,0,0.4))
        }
        
      }
    }
    for (i in 1:num_figure) {
      df <- df_list[[i]]
      pcol <- pcol_list[[i]]
      pfcol<- pfcol_list[[i]]
      if (centerzero) {
        arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
      } else {
        arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
      }
      PAXISLABELS <- df[1,1:n]
      if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
      if (axistype==2|axistype==3|axistype==5) {
        if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
          text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
      }
      VLABELS <- colnames(df)
      if (!is.null(vlabels)) VLABELS <- vlabels
      if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
        text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
      series <- length(df[[1]])
      SX <- series-2
      if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
      if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
      if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
      if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
      if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
      if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
      if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }
      
      
      for (i in 3:series) {
        xxs <- xx
        yys <- yy
        scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
        if (sum(!is.na(df[i,]))<3) { message(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
        } else {
          for (j in 1:n) {
            if (is.na(df[i, j])) { # how to treat NA
              if (na.itp) { # treat NA using interpolation
                left <- ifelse(j>1, j-1, n)
                while (is.na(df[i, left])) {
                  left <- ifelse(left>1, left-1, n)
                }
                right <- ifelse(j<n, j+1, 1)
                while (is.na(df[i, right])) {
                  right <- ifelse(right<n, right+1, 1)
                }
                xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
                yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
                xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
                yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
                if (xxleft > xxright) {
                  xxtmp <- xxleft; yytmp <- yyleft;
                  xxleft <- xxright; yyleft <- yyright;
                  xxright <- xxtmp; yyright <- yytmp;
                }
                xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
                yys[j] <- (yy[j]/xx[j])*xxs[j]
              } else { # treat NA as zero (origin)
                xxs[j] <- 0
                yys[j] <- 0
              }
            }
            else {
              xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
              yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
            }
          }
          if (is.null(pdensities)) {
            polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
          } else {
            polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2],
                    density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
          }
          points(xx[aux_array_odd]*scale[aux_array_odd], yy[aux_array_odd]*scale[aux_array_odd], pch=ptys[i-2], col=pcols[i-2])
          points(xx[aux_array_even]*scale[aux_array_even], yy[aux_array_even]*scale[aux_array_even], pch=ptys[i-2], col=rgb(0,0,0,0.2))
          # points(xx*scale, yy*scale, pch=ptys[i-2], col=rgb(0,0,0,0.2))
        }
      }
      
      for (i in 1:seg) { # complementary guide lines, dotted navy line by default
        polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
        if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
        if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
        if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
        if (axistype==1|axistype==3|axistype==4|axistype==5) {
          if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
            text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
        }
      }
      
    }
  }
  
  ######  plot  ######
  output$origami_plots <- renderUI({
    req(selected_data())
    plotOutput("plot")
  })
  
  
  
  output$plot <- renderPlot({
    req(selected_data())
    
    # Set up the layout using par
    layout(matrix(c(1,2,1,3), nrow=2), heights = c(5, 1), widths = c(1, 1))
    par(mar = c(1,2,1,2))
    
    #origami plot
    origami_plot_pairwise(data(), as.numeric(input$rows))
    
    #scale bar
    areas <- area_calculation_shiny(data())
    min_area <- min(areas)
    max_area <- max(areas)
    
    if(length(as.numeric(input$rows))==1){
      # Draw the fixed range bar plot (0 to 1)
      create_bar_plot(
        x_range = c(0, 1),
        area_value1 = areas[as.numeric(input$rows)[1]],
        area_value2 = areas[as.numeric(input$rows)[1]],
        label_text = c("0", "1")
      )
      # Draw the dynamic range bar plot (min_area to max_area)
      create_bar_plot(
        x_range = c(min_area, max_area),
        area_value1 = areas[as.numeric(input$rows)[1]],
        area_value2 = areas[as.numeric(input$rows)[1]],
        label_text = c(sprintf("%.2f", min_area), sprintf("%.2f", max_area))
      )
      
    } else {
      # Draw the fixed range bar plot (0 to 1)
      create_bar_plot(
        x_range = c(0, 1),
        area_value1 = areas[as.numeric(input$rows)[1]],
        area_value2 = areas[as.numeric(input$rows)[2]],
        label_text = c("0", "1")
      )
      # Draw the dynamic range bar plot (min_area to max_area)
      create_bar_plot(
        x_range = c(min_area, max_area),
        area_value1 = areas[as.numeric(input$rows)[1]],
        area_value2 = areas[as.numeric(input$rows)[2]],
        label_text = c(sprintf("%.2f", min_area), sprintf("%.2f", max_area))
      )
    }
    
    
  })
  
  
  #################### For Weighted Origami Plots ####################
  observeEvent(input$weighted_file, {
    req(input$weighted_file)
    if(stringr::str_ends(input$weighted_file$datapath, "csv")) {
      df <- read.csv(input$weighted_file$datapath, stringsAsFactors = FALSE)
    } else if (stringr::str_ends(input$weighted_file$datapath, "(xlsx|xls)")) {
      df <- readxl::read_excel(input$weighted_file$datapath) %>% as.data.frame()
    }
    #df <- read.csv(input$weighted_file$datapath, stringsAsFactors = FALSE)
    data(df)
  })
  # Download example CSV
  output$weighted_downloadExample <- downloadHandler(
    filename = function() {
      "sucra.csv"
    },
    content = function(file) {
      file.copy("www/sucra.csv", file)
    }
  )
  
  output$weighted_downloadExampleXLSX <- downloadHandler(
    filename = function() {
      "sucra.xlsx"
    },
    content = function(file) {
      file.copy("www/sucra.xlsx", file)
    }
  )
  
  weighted_selected_data <- eventReactive(input$weighted_submit, {
    req(length(input$weighted_rows) == 1)
    data()[as.numeric(input$weighted_rows), ]
  })
  # Validate and parse the weights
  weights <- reactive({
    req(input$weights)
    weight_vec <- as.numeric(unlist(strsplit(input$weights, ",")))
    validate(
      need(min(weight_vec)!=0, "The weight cannot be zero"),
      need(length(weight_vec) == ncol(data())-1, "Number of weights must match the number of attributes"),
      need(isTRUE(all.equal(sum(weight_vec), 1, tolerance = 1e-8)), "Sum of weights must be 1")
    )
    weight_vec
  })
  
  # # Output the weights
  # output$weightsText <- renderPrint({
  #   weights()
  # })
  
  # Print table for weighted origami required data
  output$weighted_table <- renderDT({
    req(data())
    datatable(data(), selection = list(mode = "multiple", target = "row"))
  })
  
  observeEvent(input$weighted_rows, {
    if(length(input$weighted_rows) > 1) {
      showModal(modalDialog(
        title = "Selection Error",
        "Please select exactly 1 row.",
        easyClose = TRUE,
        footer = NULL
      ))
      updateCheckboxGroupInput(session, "weighted_rows", selected = input$weighted_rows[1])
    }
  })
  
  ##############
  output$weighted_row_selector <- renderUI({
    req(data())  # Ensure 'data' exists before rendering UI
    
    # Create checkbox input and an info button within the same UI context
    tagList(
      
      div(style = "display: flex; align-items: center;",
          checkboxGroupInput("weighted_rows",
                             "Select exactly 1 from the following indices:",
                             choices = 1:nrow(data()), inline = TRUE),
          # actionButton("weighted_infoBtn", "", icon = icon("info-circle"),
          #              style = "width: 20px; height: 20px; 
          #              font-size: 10px; padding: 5px;
          #              background-color: #f8f8f8; 
          #              color: #83a9e6; border: none;"),
          
      ),
      actionButton("weighted_submit", "Submit"),
    )
  })
  
  # Observe event for the 'More Info' button to show a modal dialog
  # observeEvent(input$weighted_infoBtn, {
  #   showModal(modalDialog(
  #     title = "Additional instructions",
  #     HTML("Based on your preferences and needs, please select one of the following options:
  #     <ul class='custom-list'>
  #       <li><strong>Option 1:</strong> Select a single index from the list to create an origami plot for an individual object.</li>
  #       <li><strong>Option 2:</strong> Select exactly two indices from the list to generate a pairwise origami plot for comparing two objects.</li>
  #     </ul>"),
  #     easyClose = TRUE,
  #     footer = modalButton("Close")
  #   ))
  # })
  
  # this is for results panel
  observeEvent(input$results_plot, {
    showModal(modalDialog(
      title = "Interpret the result",
      HTML("
      <ul class='custom-list'>
        <li><strong>Origami Plot:</strong> An origami plot visualizes the values of attributes for a single object. The attributes are represented as solid axes, with values marked 
        accordingly along these axes. If the input table contains more than two rows, two scales will appear below the output figure. The left scale ranges from the minimum to the 
        maximum area of the input table, while the right scale spans from 0 to 1. A bar on the scales indicates the area of the object currently being plotted. If the input table 
        contains two or fewer rows, only one scale will be displayed below the figure.</li>
        <li><strong>Pairwise Origami Plot:</strong> A pairwise origami plot compares the values of attributes between two objects. If the title shows A vs B, object A is plotted 
        in green, while object B is plotted in red. The area scales below the figure function the same as in a standard Origami plot, with two bars indicating the areas of object 
        A (green) and object B (red).</li>
      </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  ##############
  
  # output$weighted_row_selector <- renderUI({
  #   req(data())
  #   checkboxGroupInput("weighted_rows", "Select exactly 1 row:", choices = 1:nrow(data()), inline = TRUE)
  # })
  
  
  ##############
  
  origami_plot_weighted<- function(df, index, weights, min_value=0.15, pcol= rgb(0.2,0.5,0.5,1), pfcol= rgb(0.2,0.5,0.5,0.1),
                                   pcol_weighted = rgb(0.6,0.3,0.3,1), pfcol_weighted = NULL, axistype=1, seg=4, pty=16,
                                   plty=1:6, plwd=1, pdensity=NULL, pangle=45, cglty=1.4, cglwd=0.1,
                                   cglcol="#000000", axislabcol="#808080", title="",
                                   na.itp=TRUE, centerzero=TRUE, vlabels=NULL, vlcex=1,
                                   caxislabels=seq(0,1,by = 0.25), calcex=NULL,
                                   paxislabels=NULL, palcex=NULL) {
    
    #if (sum(weight)!=1) { stop("The weight must sum up to 1\n"); return() }
    
    
    #weight <- c(0.1,0.3,0.3,0.1,0.2)
    row_names <- df[,1]
    df <- df[,2:ncol(df)]
    
    index <- as.numeric(index)
    weight <- as.numeric(weights)
    
    df <- df[index,]
    title <- row_names[index]
    df2_original = df
    
    #check if object is valid
    #if(dim(df)[1]==0){ stop("The object is not present in the dataframe. Please ensure the object matches the row name exactly.")}
    if(dim(df)[1]==0){validate(
      need(dim(df)[1]==1, "No object selected yet")
    ) }
    
    #if(dim(df)[1]>1){ stop("The object is duplicated in the dataframe. Please ensure there are no duplicate row names.")}
    #if(dim(df)[1]==1){
    #  df2_original = df
    df <- data_preparation(df,min_value = 0.15)
    #}
    
    n_prime <- ncol(df)/2
    aux_array_odd <- as.vector(seq(1,2*n_prime-1,2))
    aux_array_even <- as.vector(seq(2,2*n_prime,2))
    #df2_original <- df[3,aux_array_odd]
    
    max_weight <- max(weight)
    df2 <- df2_original * (weight / max_weight)
    #min_value <- min_value
    df2 <- data_preparation(df2, min_value = 0.15)
    
    df_list <- list(df,df2)
    
    pcol_list <- list(pcol,pcol_weighted)
    pfcol_list <- list(pfcol,pfcol_weighted)
    num_figure = 2
    
    n_col = dim(df)[2]
    if (!is.data.frame(df)) { stop("The data must  be given as dataframe.\n"); return() }
    if ((n <- length(df))<3) { stop("The number of variables must be 3 or more.\n"); return() }
    plot(c(-1.2, 1.2), c(-1.2, 1.2), type="n", frame.plot=FALSE, axes=FALSE,
         xlab="", ylab="", main=title, asp=1) # define x-y coordinates without any plot
    theta <- seq(90, 450, length=n+1)*pi/180
    theta <- theta[1:n]
    xx <- cos(theta)
    yy <- sin(theta)
    CGap <- ifelse(centerzero, 0, 1)
    points(0,0, pch = 16, col  = rgb(0,0,0,0.2))
    for (ind in 1:n_col){
      if (ind == 1){
        segments(0, 0, 0, 1, lwd = 2, lty = 1, col = rgb(0,0,0,0.2)) # factor 1
      } else{
        if ((ind %% 2) == 0){
          draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 2, col = rgb(0,0,0,0.4))
        } else{
          draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 1, col = rgb(0,0,0,0.4))
        }
        
      }
    }
    for (i in 1:num_figure) {
      
      df <- df_list[[i]]
      pcol <- pcol_list[[i]]
      pfcol<- pfcol_list[[i]]
      
      if(i==2) {
        plty = "longdash"
        pfcol <- NULL
        plwd <- 2
      }
      if (centerzero) {
        arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
      } else {
        arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
      }
      PAXISLABELS <- df[1,1:n]
      if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
      if (axistype==2|axistype==3|axistype==5) {
        if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
          text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
      }
      VLABELS <- colnames(df)
      if (!is.null(vlabels)) VLABELS <- vlabels
      if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
        text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
      series <- length(df[[1]])
      SX <- series-2
      if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
      if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
      if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
      if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
      if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
      if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
      if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }
      
      
      for (i in 3:series) {
        xxs <- xx
        yys <- yy
        scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
        if (sum(!is.na(df[i,]))<3) { message(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
        } else {
          for (j in 1:n) {
            if (is.na(df[i, j])) { # how to treat NA
              if (na.itp) { # treat NA using interpolation
                left <- ifelse(j>1, j-1, n)
                while (is.na(df[i, left])) {
                  left <- ifelse(left>1, left-1, n)
                }
                right <- ifelse(j<n, j+1, 1)
                while (is.na(df[i, right])) {
                  right <- ifelse(right<n, right+1, 1)
                }
                xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
                yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
                xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
                yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
                if (xxleft > xxright) {
                  xxtmp <- xxleft; yytmp <- yyleft;
                  xxleft <- xxright; yyleft <- yyright;
                  xxright <- xxtmp; yyright <- yytmp;
                }
                xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
                yys[j] <- (yy[j]/xx[j])*xxs[j]
              } else { # treat NA as zero (origin)
                xxs[j] <- 0
                yys[j] <- 0
              }
            }
            else {
              xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
              yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
            }
          }
          if (is.null(pdensities)) {
            polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
          } else {
            polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2],
                    density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
          }
          points(xx[aux_array_odd]*scale[aux_array_odd], yy[aux_array_odd]*scale[aux_array_odd], pch=ptys[i-2], col=pcols[i-2])
          points(xx[aux_array_even]*scale[aux_array_even], yy[aux_array_even]*scale[aux_array_even], pch=ptys[i-2], col=rgb(0,0,0,0.2))
          # points(xx*scale, yy*scale, pch=ptys[i-2], col=rgb(0,0,0,0.2))
        }
      }
      
      for (i in 1:seg) { # complementary guide lines, dotted navy line by default
        polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
        if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
        if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
        if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
        if (axistype==1|axistype==3|axistype==4|axistype==5) {
          if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
            text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
        }
      }
      
    }
  }
  
  ######  plot  ######  
  output$weighted_origami_plots <- renderUI({
    req(weighted_selected_data())
    plotOutput("weighted_plot")
  })
  
  output$weighted_plot <- renderPlot({
    req(weighted_selected_data())
    
    # Set up the layout using par
    # layout(matrix(c(1,2,1,3), nrow=2), heights = c(5, 1), widths = c(1, 1))
    # par(mar = c(1,2,1,2))
    
    layout(matrix(c(1,0,1,2,1,0), nrow=2), heights = c(5, 1), widths = c(1, 1))
    par(mar = c(1,2,1,2))
    
    #weighted origami plot
    origami_plot_weighted(data(), index = as.numeric(input$weighted_rows), weights = weights())
    
    df_temp <- data()
    nrows <- nrow(df_temp)
    ncols <- ncol(df_temp)
    
    areas <- area_calculation_shiny(data())
    weight_max <- max(weights())
    weight_std <- weights() / weight_max # this should be a vector of standardized weight
    weighted_data <- matrix(0,nrow=nrows,ncol = ncols-1)
    #sliced_data <- as.matrix(data()[2:nrow(data()),2:ncol(data())])
    
    for(i in 1:nrows){
      for(j in 1:(ncols-1)){
        weighted_data[i,j] <- df_temp[i,j+1] * weight_std[j]
      }
    }
    
    areas_weighted <- area_calculation_shiny(weighted_data)
    
    if(is.null(weights())){
      create_bar_plot(
        x_range = c(0, 1),
        area_value1 = areas[as.numeric(input$weighted_rows)[1]],
        area_value2 = areas[as.numeric(input$weighted_rows)[1]],
        label_text = c("0", "1")
      )
    } else {
      create_bar_plot(
        x_range = c(0, 1),
        area_value1 = areas[as.numeric(input$weighted_rows)[1]],
        area_value2 = areas_weighted[as.numeric(input$weighted_rows)[1]],
        label_text = c("0", "1")
      )
    }
    
    
    
  })
  
  
  
  #################### Other Functions  #################### 
  ##### Data Preparation #####
  data_preparation <- function(df, min_value = NULL, ...){
    df_names <- colnames(df)
    n_prime <- ncol(df)
    if(is.null(min_value)){
      min_value <- as.numeric(min(df)/2)
    }
    df <- data.frame(mapply(cbind, df, "aux"=min_value, SIMPLIFY=F))
    aux_array_even <- seq(2,2*n_prime,2)
    aux_array_odd <- seq(1,2*n_prime-1,2)
    colnames(df)[aux_array_even]=""
    colnames(df)[aux_array_odd]=df_names
    df <- as.data.frame(rbind(1,0,df))
    return(df)
  }
  
  ##### Area Calculation #####
  area_calculation_shiny <- function(df){
    df <- df[,2:ncol(df)]
    result <- c()
    for(i in 1:nrow(df)){
      area <- sum(df[i,])/ncol(df)
      area <- round(area,digits = 2)
      result <- c(result,area)
    }
    return(result)
  }
  
  ##### plot for scale bars #####
  create_bar_plot <- function(x_range, color_range=c("#EBF5EE","#D8BFAA"), area_value1, area_value2, label_text) {
    colors <- colorRampPalette(color_range)(1000)
    plot(NULL, NULL, xlim = c(x_range[1], x_range[2]), ylim = c(-0.3, 0.1), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    gradient.rect(x_range[1], -0.1, x_range[2], 0.1, col = colors, gradient='x', border = NA)
    segments(x0 = area_value1, y0 = -0.1, x1 = area_value1, y1 = 0.1, col = rgb(0.2,0.5,0.5,1), lwd = 2,)
    segments(x0 = area_value2, y0 = -0.1, x1 = area_value2, y1 = 0.1, col = rgb(0.6,0.3,0.3,1), lwd = 2,)
    text(x_range[1], -0.06, labels = label_text[1], pos = 1, offset = 1)
    text(x_range[2], -0.06, labels = label_text[2], pos = 1, offset = 1)
    text(area_value1, -0.06, labels = sprintf("%.2f", area_value1), pos = 1, offset = 1, col = rgb(0.2,0.5,0.5,1))
    text(area_value2, -0.06, labels = sprintf("%.2f", area_value2), pos = 1, offset = 1, col = rgb(0.6,0.3,0.3,1))
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)