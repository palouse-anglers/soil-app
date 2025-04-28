# soil_chat_server.R
soil_chat_server <- function(id, database_path = "soil_data3.sqlite") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    extracted_query <- reactiveVal("")
    database_filter <- reactiveVal(data.frame())
    
    # Initialize Chatbot with System Prompt
    chat <- ellmer::chat_openai(
      system_prompt = "
You are a SQL assistant working with a SQLite database called 'soil_data3'.

Available columns:
- parameter (TEXT), sample_date (DATE), field_name (TEXT), field_id (TEXT), year (INTEGER), hc12_name (TEXT).

Valid parameters include examples like 'Al (ppm)', 'SO4S (ppm)', 'pH 1:1', etc.

Rules:
- Only create valid SELECT queries.
- Use LOWER(parameter) LIKE '%ph%' for fuzzy matching if pH mentioned.
- Always ensure result IS NOT NULL when doing calculations.
- If no matching data, respond that no results were found.
- Never use INSERT, UPDATE, DELETE, DROP, CREATE.
- Only return plain SQL without markdown formatting.
"
    )
    
    # Helper function to execute SQL query
    execute_query <- function(query) {
      tryCatch({
        db <- dbConnect(RSQLite::SQLite(), database_path)
        on.exit(dbDisconnect(db), add = TRUE)
        dbGetQuery(db, query)
      }, error = function(e) {
        warning("Error executing query: ", e)
        NULL
      })
    }
    
    
    # Hide chat_ui initially
    shinyjs::hide(id = "chat_box")
    shinyjs::hide(id = "robot_emoji_box")
    
    observeEvent(input$wake_up, {
      if (input$wake_up) {
        shinyjs::show(id = "chat_box")
        shinyjs::show(id = "robot_emoji_box")
      } else {
        shinyjs::hide(id = "chat_box")
        shinyjs::show(id = "robot_emoji_box")
      }
    })
    
    output$robot_emoji <- renderText({
      req(input$wake_up)
      "🤖 Table Bot is awake!
      Ask questions to generate tables
      How many total records are in the soil dataset?
      Show average pH  1:1 per huc 12 watershed
      "
    })
    
    # When user submits chat input
    observeEvent(input$chat_user_input, {
      
      req(input$wake_up)
      
      user_input <- input$chat_user_input
      
      chat$chat_async(paste("Generate an SQL query for:", user_input)) %...>% 
        (function(response) {
          query <- trimws(gsub("```.*?```", "", response))
          
          extracted_query(query)  # Save extracted SQL
          
          if (!grepl("^SELECT", toupper(query))) {
            chat_append(ns("chat"), "❌ Error: LLM did not generate a valid SELECT query.")
            database_filter(data.frame())
            return(NULL)
          }
          
          future({
            execute_query(query)
          }) %...>%
            (function(result) {
              if (is.null(result)) {
                chat_append(ns("chat"), "❌ Error executing SQL query.")
                database_filter(data.frame())
              } else if (is.data.frame(result) && nrow(result) == 0) {
                chat_append(ns("chat"), "⚠️ Query executed but returned no results.")
                database_filter(data.frame())
              } else {
                chat_append(ns("chat"), "✅ Query executed successfully. Results shown below.")
                database_filter(result)
              }
            }) %...!%
            (function(e) {
              chat_append(ns("chat"), "❌ Error during query execution.")
            })
        })
    })
    
    # Render outputs
    output$query_text <- renderText({
      req(extracted_query() != "")
      paste0("📝 SQL Query Generated:\n\n", extracted_query())
    })
    
    output$result_table <- DT::renderDT({
      req(nrow(database_filter()) > 0)
      DT::datatable(database_filter(), options = list(pageLength = 10))
    })
  })
}
