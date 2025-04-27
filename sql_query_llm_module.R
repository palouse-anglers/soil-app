# sql_query_llm_module.R

# --------------------
# UI Function
# --------------------
sql_query_llm_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    chat_ui(ns("chat")),
    verbatimTextOutput(ns("query_text")),
    tableOutput(ns("result_table"))
  )
}

# --------------------
# Server Function
# --------------------
sql_query_llm_server <- function(id, database_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plan(multisession)  # async inside module is safe
    
    # Initialize Chat Model
    chat <- ellmer::chat_openai(
      system_prompt = "
You are an SQL assistant working with a SQLite database called 'soil_data3'.

Available columns:
- parameter (TEXT)
- sample_date (DATE)
- field_name (TEXT)
- field_id (TEXT)
- year (INTEGER)
- hc12_name (TEXT)

⚡ List of valid parameter values (case-sensitive):
'Al (ppm)', 'Al(KCl) (ppm)', 'Avl H2O % (%)', 'Avl H2O (inches)', 'B (ppm)', 'Bray P(1:10) (ppm)', 
'CEC (meq/100g)', 'Ca (KCl) (ppm)', 'Cl (ppm)', 'Cu (ppm)', 'Density (g/ml)', 
'Density 2 (mill-lbs/ac-depth)', 'Ec(1:1) (dS/m)', 'Efferves (Scale 0-7)', 
'Est CEC (meq/100g)', 'Est. SS (dS/m)', 'Exch Ca (meq/100g)', 'Exch K (meq/100g)', 
'Exch Mg (meq/100g)', 'Exch Na (meq/100g)', 'Fe (ppm)', 'Field Capacity', 
'H2O wet/dry', 'Mg (KCl) (ppm)', 'Mn (ppm)', 'NH4N (ppm)', 'NH4N# (lbs/ac-depth)', 
'NO3N (ppm)', 'NO3N# (lbs/ac-depth)', 'Na (KCl) (ppm)', 'OM (%)', 'Olsen K (ppm)', 
'Olsen P (ppm)', 'SO4S (ppm)', 'SO4S# (lbs/ac-depth)', 'Soil Texture', 
'Total Bases (meq/100g)', 'Wilting Point', 'Zn (ppm)', 'pH (A-E)', 'pH 1:1'

Rules:
- Only generate SELECT queries.
- No INSERT, UPDATE, DELETE, DROP, ALTER.
- Values like 'pH', 'SO4S' are inside 'parameter'.
- Output only plain SQL (no markdown).
- If unsure, reply: 'Unable to generate a valid SQL query.'

Examples:
SELECT COUNT(*) FROM soil_data3;
SELECT COUNT(DISTINCT parameter) FROM soil_data3;
SELECT field_name, MAX(result) FROM soil_data3 WHERE parameter = 'pH';
SELECT parameter, MAX(result) FROM soil_data3 GROUP BY parameter;
SELECT * FROM soil_data3 WHERE year BETWEEN 2018 AND 2020 ORDER BY sample_date;
      "
    )
    
    extracted_query <- reactiveVal("")
    database_filter <- reactiveVal(data.frame())
    
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
    
    observeEvent(input$chat_user_input, {
      user_input <- input$chat_user_input
      
      chat$chat_async(paste("Generate an SQL query for:", user_input)) %...>%
        (function(response) {
          query <- trimws(gsub("```.*?```", "", response))
          
          cat("LLM Response:", response, "\n")
          cat("Extracted SQL:", query, "\n")
          
          extracted_query(query)
          
          if (!grepl("^SELECT", toupper(query))) {
            chat_append(ns("chat"), "❌ Error: Not a valid SELECT SQL query.")
            database_filter(data.frame())
            return(NULL)
          }
          
          future({
            execute_query(query)
          }) %...>%
            (function(result) {
              if (is.null(result) || nrow(result) == 0) {
                chat_append(ns("chat"), "⚠️ Query executed but returned no results.")
                database_filter(data.frame())
              } else {
                chat_append(ns("chat"), "✅ Query successful. Results displayed.")
                database_filter(result)
              }
            }) %...!%
            (function(e) {
              chat_append(ns("chat"), "❌ Error executing SQL query.")
            })
          
        })
    })
    
    output$query_text <- renderText({
      req(extracted_query() != "")
      paste0("📝 SQL Query Generated:\n\n", extracted_query())
    })
    
    output$result_table <- renderTable({
      req(database_filter())
      database_filter()
    })
    
  })
}
