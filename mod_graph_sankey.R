mod_graph_sankey_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    id,
    
    shinydashboardPlus::box(
        width = 12,
        title = "File Upload (Node ID variable must be successive integers start from 0)",
        collapsible = TRUE,
        dropdownMenu = reset_dropdown(ns("reset_file")),
        fluidRow(
          column(6, fileInput(ns("sankey"), "Choose an input file", 
                              accept = file_accept))
        ),
        fluidRow(
          column(2, selectInput(ns("sankey_node"),   "Node",    NULL)),
          column(2, selectInput(ns("sankey_node_id"), "Node ID", NULL)),
          column(2, selectInput(ns("sankey_source"), "Source",  NULL)),
          column(2, selectInput(ns("sankey_target"), "Target",  NULL)),
          column(2, selectInput(ns("sankey_value"),  "Value",   NULL))
        )
        
    ),
    shinydashboardPlus::box(width = 12,
        title = "Plot Parameter",
        collapsible = TRUE,
        fluidRow(
          column(3, numericInput(ns('height'), "Plot Height", 400, min=400, max=8000, step=100)),
          column(3, numericInput(ns('width'), "Plot Width", 900, min=400, max=2000, step=100)),
          column(3, numericInput(ns('node_weight'), "Node Weight", 10, min=1, max=20, step=1)),
          column(3, numericInput(ns('pad'), "Node Pad", 15, min=5, max=30, step=1)),
          column(3, numericInput(ns('transparency'), "Transparency", 0.6, min=0.1, max=1, step=0.1)),
          column(3, radioButtons(ns("display_n"), "Display Node Value", choices = list("Yes" = 1, "No" = 2), selected = 2, inline=T)),
          column(6, textInput(ns('title'), "Title", NULL))
        )
    ),
    shinydashboardPlus::box(width = 12,
                            collapsible = TRUE,
                            title = 'Customize the Colors for Sources',
                            fluidRow(
                              column(12, uiOutput(ns("color_input")))
                            )
    ),
    
    tags$head(
      tags$script(HTML('
Shiny.addCustomMessageHandler("addDraggableText", function(message) {
    var text = message.text.substr(9);
    var x = message.x;
    var y = message.y;
    var id = message.id;
  //console.log(message);
  //console.log(message.id);

    var newText = document.createElement("div");
      newText.style.position = "absolute";
      newText.style.left = x + "px";
      newText.style.top = y + "px";
      newText.style.cursor = "move";
      newText.style.fontSize = 18 + "px";
      newText.innerText = text;
      newText.id = id;
    console.log(newText);

    newText.addEventListener("mousedown", function(e) {
      var posX = e.clientX;
      var posY = e.clientY;
      document.onmousemove = function(e) {
        var dx = e.clientX - posX;
        var dy = e.clientY - posY;
        posX = e.clientX;
        posY = e.clientY;
        newText.style.left = (newText.offsetLeft + dx) + "px";
        newText.style.top = (newText.offsetTop + dy) + "px";
      }
      document.onmouseup = function() {
        document.onmousemove = document.onmouseup = null;
      }
    });
    console.log(newText);


  // Add right-click menu
  newText.addEventListener("contextmenu", function(e) {
    e.preventDefault();
    var menu = document.createElement("div");
    menu.id = "contextMenu";
    menu.style.position = "absolute";
    menu.style.left = newText.offsetLeft + "px";
    menu.style.top = newText.offsetTop + "px";
    menu.style.background = "#C8E2FA";
    menu.style.border = 1 + "px";
    console.log(menu);
    

    var fontSizeOption = document.createElement("div");
    fontSizeOption.innerText = "Change Font Size";
    fontSizeOption.addEventListener("click", function() {
      var inputContainer = document.createElement("div");
      inputContainer.style.position = "absolute";
      inputContainer.style.left = newText.offsetLeft + "px";
      inputContainer.style.top = newText.offsetTop + 5 + "px";
      inputContainer.style.display = "flex";
      inputContainer.style.flexDirection = "column";
      inputContainer.style.maxWidth = "200px"; // Set a maximum width for the input container

      var newSizeInput = document.createElement("input");
      newSizeInput.type = "number";
      newSizeInput.value = 18;
      newSizeInput.placeholder = "Enter new font size";
      newSizeInput.style.width = "100%"; // Make the input box take 100% width of the container
      newSizeInput.style.marginBottom = "5px";

      var confirmButton = document.createElement("button");
      confirmButton.innerText = "Confirm";
      confirmButton.style.width = "100%"; // Make the button take 100% width of the container
      confirmButton.addEventListener("click", function() {
        var newSize = newSizeInput.value;
        if (newSize !== "") {
          newText.style.fontSize = newSize + "px";
        }
        inputContainer.remove();
        menu.remove(); // Remove the context menu
      });

      inputContainer.appendChild(newSizeInput);
      inputContainer.appendChild(confirmButton);
      console.log(inputContainer);

      document.getElementById("g_sankey-sankey_plot").appendChild(inputContainer);
    });

    var removeOption = document.createElement("div");
    removeOption.innerText = "Remove Text";
    removeOption.addEventListener("click", function() {
      newText.remove();
      menu.remove(); // Remove the context menu
    });

    menu.appendChild(fontSizeOption);
    menu.appendChild(removeOption);
    document.getElementById("g_sankey-sankey_plot").appendChild(menu);
  });

    document.getElementById("g_sankey-sankey_plot").appendChild(newText);
});
'))
),
    
    titlePanel("Draggable Text on Graph"),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("text"), "Enter Text"),
        actionButton(ns("addText"), "Add Text to Graph"),
        textOutput(ns("removeInstruction")),
      ),
    
      shinydashboardPlus::box(width = 12,
                              title = "Sankey Plot",
                              plotlyOutput(ns("sankey_plot"))
      )
    )
  )
}



mod_graph_sankey_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
      
      rv <- reactiveValues(
        df_sankey = NULL,
      )
      

      #Function to create Tepee plot
      sankey <- function(indata)
      {
        #var1 <- input$sankey_node
        #print(var1)

        #input_data <- indata
        input_data <- indata %>%
          mutate(node := !!sym(input$sankey_node)) %>%
          mutate(node_id := !!sym(input$sankey_node_id)) %>%
          mutate(source := !!sym(input$sankey_source)) %>%
          mutate(target := !!sym(input$sankey_target)) %>%
          mutate(value := !!sym(input$sankey_value))          

        input_data <- select(input_data, node, node_id, source, target, value)

        
        #######################################################################################
        ##################################### Create plot #####################################
        #######################################################################################
        # input_data$source contains integers which represent node, need to get node names to be displayed in
        # color selection section, so user knows which node he/she is choosing color for
        node_name <- input_data %>%
                     select(node_id, node) %>%
                     subset(node!='' & !is.na(node)) %>%
                     arrange(node_id)

        # This data frame contains all node which may not be source
        source_name <- rename(node_name, source=node_id)

        # After merge, this data frame contains only the node which can be source
        source_name <- merge(source_name, select(input_data, source), by='source')

        
        source_name_dedup <- source_name %>% 
          distinct(source, .keep_all = TRUE) %>%
          arrange(source)

        # Node can relate to different Node ID, this happens when a therapy (represented by node) can be displayed at
        # the 1st LOT, 2nd LOT, etc. In this case, links flow from them will use the same color. 
        funs <- unique(source_name$node)
        

        
        default_color <- colors()
        cols <- reactive({
          lapply(seq_len(length(funs)), function(i) {
            column(3, colourInput(ns(paste("col", i, sep="_")), funs[i], default_color[i])   )     
          })
        })
        
        output$color_input <- renderUI({cols()})  
        
        # Put all the input in a vector
        colors2 <- reactive({
          lapply(seq_len(length(funs)), function(i) {
            input[[paste("col", i, sep="_")]]
          })
        })
        
        updated_color <- unlist(colors2())
        req(updated_color)
        
        default_color <- updated_color
        if (length(default_color) != length(funs)) {default_color<-colors()}
        
        
        updated_color2 <- paste(sapply(updated_color, function(x) 
                                       {paste0("rgba(", paste(c(col2rgb(x), input$transparency), collapse = "," ), ")") }
                                ), collapse = ", ")
        
        updated_color3 <- vector(length = length(funs))
        test <- strsplit(updated_color2, split=', ')
        for (i in 1:length(funs)) {
          updated_color3[i] <- test[[1]][i]
        }

        color_data <- data.frame(node=unique(source_name$node),
                                 link_color=updated_color3)
        # Due to the fact that node can relate to different Node ID, in the above data frame, we use
        # node=unique(source_name$node) instead of source=unique(source_name$source)
        # Now, need to merge back to get source (which are integers)
        color_data <- merge(color_data, source_name_dedup, by='node')

        input_data <- merge(input_data, select(color_data, -c('node')), by='source', all.x=T)
        
        
        #########################################################################################################
        ##################################### Create Node Value For Display #####################################
        #########################################################################################################
        source_node_value <- input_data %>%
                             group_by(source) %>%
                             summarise(node_value = sum(value)) %>%
                             rename(node_id = source) %>%
                             ungroup()

        target_node_value <- input_data %>%
                             group_by(target) %>%
                             summarise(node_value = sum(value)) %>%
                             rename(node_id = target) %>%
                             ungroup()
        
        node_value <- rbind(source_node_value, target_node_value)

        node_value_dedup <- node_value %>%
                            distinct(node_id, .keep_all = TRUE)

        #write.csv(node_value_dedup, 'C:/Users/huangj74/Downloads/node_value_dedup.csv')
        #write.csv(node_name, 'C:/Users/huangj74/Downloads/node_name.csv')
        
        node_name <- merge(node_name, node_value_dedup, by='node_id')
        
        if (input$display_n == 1) {
          node_name <- mutate(node_name, node=paste0(node, ' ', node_value))
        }
        

        # Create plot
        p <- plot_ly(
          type = "sankey",
          # domain = c(
          #   x =  c(0,1),
          #   y =  c(0,1)
          # ),
          orientation = "h",
          valueformat = ".0f",
          valuesuffix = " ",
          
          node = list(
            label = node_name$node,
            color = 'black',
            pad = input$pad,
            thickness = input$node_weight,
            line = list(
              color = "black",
              width = 0.5
            )
          ),
          
          link = list(
            source = input_data$source,
            target = input_data$target,
            value =  input_data$value,
            color =  input_data$link_color
          )
        ) %>% 
          layout(
            title = input$title,
            height = input$height,
            width = input$width,
            font = list(
              size = 12
            ),
            xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
            yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
          )
        
          p
      }
      #End of the plot function
      
      
      observe({
        req(input$sankey)
        rv$df_sankey <- upload_file(input$sankey)
      })
      
      # Reset fileInputs
      observeEvent(input$reset_file, {
        rv$df_sankey <- NULL
        reset("sankey")
      })
      
      main1 <- reactive({
        req(rv$df_sankey)
        process_file(rv$df_sankey)
      })
      
      observeEvent(input$sankey, {
        var_name <- names(rv$df_sankey)
        default_node      <- grep("^la", var_name, TRUE, value = TRUE)
        default_node_id   <- grep("^lid", var_name, TRUE, value = TRUE)
        default_source    <- grep("^s", var_name, TRUE, value = TRUE)
        default_target    <- grep("^t", var_name, TRUE, value = TRUE)
        default_value     <- grep("^v", var_name, TRUE, value = TRUE)
        
        updateSelectInput(session, "sankey_node",    choices = var_name, selected=default_node)
        updateSelectInput(session, "sankey_node_id", choices = var_name, selected=default_node_id)
        updateSelectInput(session, "sankey_source",  choices = var_name, selected=default_source)
        updateSelectInput(session, "sankey_target",  choices = var_name, selected=default_target)
        updateSelectInput(session, "sankey_value",   choices = var_name, selected=default_value)
      })
        
      output$removeInstruction <- renderText({
        "Right-click on the text to access the context menu for additional options."
      })
      
      ##############################################################################################################
      ###Generate Plot Parameter Input Box (X-axis range, step by value) depend on which efficacy data you upload###
      ##############################################################################################################
      output$sankey_plot <- renderPlotly({
        sankey_data <- main1()
        req(sankey_data)
        sankey(indata=sankey_data)
      })

      
      observeEvent(input$addText, {
        text <- input$text
        x <- 10
        y <- 10
        id_ <- paste0("text_", format(Sys.time(), "%H%M%S"))

        if (nchar(text) > 0) {
          session$sendCustomMessage("addDraggableText", list(text = session$ns(text), x = 10, y = 10, id = session$ns(id_)))
        }
      })
    })
}