# plot_bot_server <- function(id,soil_data3) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
# 
#     # Hide initially
#     shinyjs::hide(id = "chat_box")
#     shinyjs::hide(id = "robot_emoji_box")
#     
#     observeEvent(input$wake_up, {
#       if (input$wake_up) {
#         shinyjs::show(id = "chat_box")
#         shinyjs::show(id = "robot_emoji_box")
#       } else {
#         shinyjs::hide(id = "chat_box")
#         shinyjs::hide(id = "robot_emoji_box")
#       }
#     })
#     
#     output$robot_emoji <- renderText({
#       req(input$wake_up)
#       "🤖 Plot Bot is awake!
#       Try asking:
#       • Plot average pH by year
#       • Boxplot of NO3N (ppm) by field
#       • Compare pH in Whiskey Creek vs McKay Creek"
#     })
#     
#     # Reactive values
#     generated_code <- reactiveVal("")  # Store last R code
#     generated_plot <- reactiveVal(NULL)  # Store last ggplot
#     
#     # Initialize Plotting Chatbot
#     chat <- ellmer::chat_openai(
#       system_prompt = "
# You are an R plotting assistant working with an R dataframe called 'soil_data3'.
# 
# Data are long format with columns:
# - parameter (TEXT): Soil chemistry parameter name.
# - result (REAL): Measured value.
# - sample_date (DATE): Date of sampling.
# - field_name (TEXT): Name of the farm field.
# - field_id (TEXT): Field ID.
# - year (INTEGER): Year sample was collected.
# - hc12_name (TEXT): Watershed name.
# 
# Valid parameter values (case-sensitive) include: 'Al (ppm)', 'SO4S (ppm)', 'pH 1:1', 'NO3N (ppm)', etc.
# 
# Rules:
# - Create valid R ggplot2 or plotly plots based on user request.
# - Filter correctly by parameter, field_name, or year when needed.
# - Always use 'result' as the y-axis for plots involving values.
# - Remove NA results when plotting.
# - Output R code only (no SQL, no markdown formatting like ```).
# - Use plotly::ggplotly() where possible to make plots interactive.
# - If a user says 'farm name' assume they mean field_name.
# - If a user says 'huc 12' or watershed assume they mean hc12_name.
# 
# If you cannot create a valid plot, respond: 'Unable to generate a plot based on the request.'
# 
# Example user prompts:
# - 'Plot average pH 1:1 by year.'
# - 'Scatter plot of SO4S vs NO3N for all fields.'
# - 'Boxplot of pH grouped by field_name.'
# - 'Density plot of OM (%) across all fields.'
# 
# If a user requests 'number of samples' or 'total samples' assume they mean a count of rows grouped by the appropriate field (e.g., year, field_name, hc12_name).
# 
# Example:
# User: 'Plot total samples by year'
# You should create:
# soil_data3 %>%
#   count(year) %>%
#   ggplot(aes(x = year, y = n)) +
#   geom_col() +
#   labs(title = 'Number of Samples by Year'', x = 'year'', y = 'Number of Samples') +
#   theme_minimal() %>%
#   ggplotly()
# 
# "
#     )
#     
#     # Evaluate R code safely
#     safe_eval <- function(code_string) {
#       tryCatch({
#         eval_env <- new.env(parent = globalenv())
#         eval_env$soil_data3 <- soil_data3  # Inject manually
#         
#         expr <- parse(text = code_string)
#         result <- eval(expr, envir = eval_env)
#         
#         if (inherits(result, "plotly") || inherits(result, "gg")) {
#           if (inherits(result, "gg")) {
#             result <- plotly::ggplotly(result)
#           }
#           return(result)
#         } else {
#           warning("Generated code did not return a recognized plot.")
#           NULL
#         }
#       }, error = function(e) {
#         warning("Error evaluating generated R code: ", e$message)
#         NULL
#       })
#     }
#     
#     # React to user chat input
#     observeEvent(input$chat_plot_user_input, {
#       user_input <- input$chat_plot_user_input
#       
#       chat$chat_async(paste("Generate R code for:", user_input)) %...>% 
#         (function(response) {
#           code <- trimws(gsub("```.*?```", "", response))  # clean
#           cat("LLM Response:", response, "\n")
#           
#           if (code == "" || grepl("provide details", code, ignore.case = TRUE)) {
#             chat_append("chat_plot", "⚠️ No valid R code generated. Please try rewording your request.")
#             generated_plot(plotly::plotly_empty())
#             generated_code("No valid R code generated.")
#           } else {
#             generated_code(code)
#             result_plot <- safe_eval(code)
#             
#             if (is.null(result_plot)) {
#               chat_append("chat_plot", "⚠️ Unable to generate a valid plot. Please try rephrasing your request.")
#               generated_plot(plotly::plotly_empty())
#             } else {
#               chat_append("chat_plot", "✅ Plot generated successfully.")
#               generated_plot(result_plot)
#             }
#           }
#         })
#     })
#     
#     
#     # Render extracted R code
#     output$r_code_text <- renderText({
#       req(generated_code() != "")
#       paste0("🧠 R Code Generated:\n\n", generated_code())
#     })
#     
#     # Render plot
#     output$plot_output <- renderPlotly({
#       req(generated_plot())
#       plotly::ggplotly(generated_plot())
#     })
#     
#     
#   })
# }

plot_bot_server <- function(id,soil_data3) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    generated_code <- reactiveVal("")
    generated_plot <- reactiveVal(NULL)
    
    # Initialize Chatbot
    chat <- ellmer::chat_openai(
      system_prompt = "
You are an R plotting assistant working with an R dataframe called 'soil_data3'.

Data are long format with columns:
- parameter (TEXT): Soil chemistry parameter name.
- result (REAL): Measured value.
- sample_date (DATE): Date of sampling.
- field_name (TEXT): Name of the farm field.
- field_id (TEXT): Field ID.
- year (INTEGER): Year sample was collected.
- hc12_name (TEXT): Watershed name.

Valid parameter values (case-sensitive) include: 'Al (ppm)', 'SO4S (ppm)', 'pH 1:1', 'NO3N (ppm)', etc.

Rules:
- Create valid R ggplot2 or plotly plots based on user request.
- Filter correctly by parameter, field_name, or year when needed.
- Always use 'result' as the y-axis for plots involving values.
- Remove NA results when plotting.
- Output R code only (no SQL, no markdown formatting like ```).
- Use plotly::ggplotly() where possible to make plots interactive.
- If a user says 'farm name' assume they mean field_name.
- If a user says 'huc 12' or watershed assume they mean hc12_name.

If you cannot create a valid plot, respond: 'Unable to generate a plot based on the request.'

Example prompts:
- 'Plot average pH 1:1 by year.'
- 'Scatter plot of SO4S vs NO3N for all fields.'
- 'Boxplot of pH grouped by field_name.'
- 'Density plot of OM (%) across all fields.'
"
    )
    
    # Safe evaluation function
    safe_eval <- function(code_string) {
      tryCatch({
        expr <- parse(text = code_string)
        result <- eval(expr)
        if (inherits(result, "plotly") || inherits(result, "gg")) {
          return(result)
        } else {
          warning("Generated code did not return a valid plot.")
          NULL
        }
      }, error = function(e) {
        warning("Error evaluating generated R code: ", e$message)
        NULL
      })
    }
    
    # React to chat input
    observeEvent(input$chat_plot_user_input, {
      user_input <- input$chat_plot_user_input
      
      chat$chat_async(paste("Generate R code for:", user_input)) %...>% 
        (function(response) {
          code <- trimws(gsub("```.*?```", "", response))
          
          cat("LLM Response:\n", response, "\n")
          cat("Extracted R Code:\n", code, "\n")
          
          generated_code(code)
          
          result_plot <- safe_eval(code)
          
          if (is.null(result_plot)) {
            chat_append(ns("chat_plot"), "⚠️ Unable to generate a valid plot. Please rephrase your request.")
            generated_plot(NULL)
          } else {
            chat_append(ns("chat_plot"), "✅ Plot generated successfully.")
            generated_plot(result_plot)
          }
        })
    })
    
    # Render extracted R code
    output$r_code_text <- renderText({
      req(generated_code())
      paste0("🧠 R Code Generated:\n\n", generated_code())
    })
    
    # Render generated plot
    output$plot_output <- renderPlotly({
      req(generated_plot())
      plotly::ggplotly(generated_plot())
    })
  })
}


