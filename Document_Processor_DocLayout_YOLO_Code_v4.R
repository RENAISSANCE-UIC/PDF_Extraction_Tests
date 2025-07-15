# PDF Document Processing Pipeline
# Extracts structured text from academic PDFs using YOLO object detection and OCR

# Load required libraries
pacman::p_load(
  tidyverse,
  magick,
  pdftools,  
  reticulate,
  ellmer,
  glue,
  here,
  R6,
  textclean
)

# ==== MAIN PROCEESING, LLM-enhanced document parsing via ellmer ====

GenericDocumentProcessor <- R6::R6Class("GenericDocumentProcessor",
                                        public = list(
                                          process_document = function(pdf_path, 
                                                                       model = "llama3.1", 
                                                                       cleaning_method = c("custom_aggressive", 
                                                                                           "textclean_academic", 
                                                                                           "hybrid", "none")) {
                                            
                                            cleaning_method <- match.arg(cleaning_method)
                                            
                                            # Extract raw text with improved column handling
                                            raw_df <- simple_process_document(pdf_path)
                                            
                                            if (is.null(raw_df)) {
                                              message("Failed to extract raw text")
                                              return(NULL)
                                            }
                                            
                                            # Enhanced preprocessing with better region identification
                                            first_page_regions <- self$prepare_regions_for_analysis(raw_df)
                                            
                                            # Use LLM to classify the document structure
                                            structure_analysis <- self$analyze_document_structure(first_page_regions, model)
                                            
                                            # Extract components based on LLM analysis
                                            title <- self$extract_title_with_llm(raw_df, structure_analysis, model)
                                            authors <- self$extract_authors_with_llm(raw_df, structure_analysis, model)
                                            abstract <- self$extract_abstract_with_llm(raw_df, structure_analysis, model)
                                            
                                            # Get main content from all pages (with improved column handling)
                                            main_text <- self$extract_main_text_smart(raw_df)
                                            
                                            # Apply cleaning based on method choice
                                            if (cleaning_method == "custom_aggressive") {
                                              main_text_cleaned <- self$clean_text_for_llm_aggressive(main_text)
                                              title_cleaned <- self$clean_text_for_llm_aggressive(title)
                                              abstract_cleaned <- self$clean_text_for_llm_aggressive(abstract)
                                            } else if (cleaning_method == "textclean_academic") {
                                              main_text_cleaned <- self$clean_text_with_textclean_academic(main_text)
                                              title_cleaned <- self$clean_text_with_textclean_academic(title)
                                              abstract_cleaned <- self$clean_text_with_textclean_academic(abstract)
                                            } else if (cleaning_method == "hybrid") {
                                              main_text_cleaned <- self$clean_text_hybrid(main_text)
                                              title_cleaned <- self$clean_text_hybrid(title)
                                              abstract_cleaned <- self$clean_text_hybrid(abstract)
                                            } else {
                                              # No cleaning
                                              main_text_cleaned <- main_text
                                              title_cleaned <- title
                                              abstract_cleaned <- abstract
                                            }

                                            # Create summary
                                            summary <- list(
                                              title = title,
                                              title_cleaned = title_cleaned,
                                              authors = authors,
                                              abstract = abstract,
                                              abstract_cleaned = abstract_cleaned,
                                              main_text = main_text,
                                              main_text_cleaned = main_text_cleaned,
                                              structure_analysis = structure_analysis,
                                              total_pages = max(raw_df$page),
                                              total_text_regions = nrow(raw_df),
                                              raw_data = raw_df,
                                              cleaning_method = cleaning_method
                                            )
                                            
                                            # If using "both", add textclean versions
                                            if (cleaning_method == "both") {
                                              summary$main_text_textclean <- main_text_textclean
                                              summary$title_textclean <- title_textclean
                                              summary$abstract_textclean <- abstract_textclean
                                            }
                                            
                                            # Save results
                                            output_file <- glue("{pdf_path}_llm_processed.RData")
                                            save(summary, file = output_file)
                                            message(glue("✓ LLM-processed results saved to {output_file}"))
                                            
                                            return(summary)
                                          },
                                          
                                          prepare_regions_for_analysis = function(raw_df) {
                                            first_page_regions <- raw_df %>%
                                              filter(page == 1) %>%
                                              arrange(y, x) %>%
                                              mutate(
                                                region_id = row_number(),
                                                text_preview = ifelse(
                                                  nchar(text) <= 400, 
                                                  str_replace_all(text, "\\s+", " "),
                                                  str_trunc(str_replace_all(text, "\\s+", " "), 400)
                                                ),
                                                element_hint = case_when(
                                                  y < 600 & nchar(text) > 50 & nchar(text) < 300 & (w * h) > 50000 ~ "likely_title",
                                                  str_detect(text, "\\b[A-Z][a-z]+ [A-Z]\\. [A-Z][a-z]+\\b|\\b[A-Z][a-z]+ [A-Z][a-z]+\\b") & y < 1000 ~ "likely_author",
                                                  str_detect(tolower(text), "abstract|summary") | (nchar(text) > 500 & y > 600 & y < 2000) ~ "likely_abstract",
                                                  str_detect(tolower(text), "^1\\.|introduction") ~ "likely_introduction",
                                                  str_detect(tolower(text), "environmental significance") ~ "likely_env_sig",
                                                  str_detect(text, "doi|cite|received|accepted|rsc\\.li") ~ "likely_citation",
                                                  (w * h) < 10000 ~ "likely_metadata",
                                                  TRUE ~ "unknown"
                                                ),
                                                text_length = nchar(text),
                                                area = w * h
                                              ) %>%
                                              select(region_id, text_preview, element_hint, text_length, area)
                                            
                                            return(first_page_regions)
                                          },
                                          
                                          analyze_document_structure = function(first_page_regions, model) {
                                            regions_text <- first_page_regions %>%
                                              mutate(
                                                description = glue("Region {region_id} [{element_hint}] ({text_length} chars, area:{area}): {text_preview}")
                                              ) %>%
                                              pull(description) %>%
                                              paste(collapse = "\n\n")
                                            
                                            system_prompt <- "You are an expert at analyzing academic paper structure. Analyze the text regions and identify content types. Pay attention to duplicate regions and select the best representative for each content type."
                                            
                                            user_prompt <- glue("
Analyze these text regions from an academic paper's first page:

{regions_text}

Identify regions containing:
- TITLE: Main paper title (avoid duplicates)
- AUTHORS: Author names and affiliations
- ABSTRACT: Paper abstract (may span multiple regions, avoid duplicates)
- INTRODUCTION: Introduction section
- OTHER: Citation, DOI, environmental significance, etc.

Respond in JSON format:
{{
  \"title_regions\": [1],
  \"author_regions\": [4],
  \"abstract_regions\": [5, 9],
  \"introduction_regions\": [10],
  \"other_regions\": {{\"3\": \"citation\", \"6\": \"publication_info\", \"8\": \"environmental_significance\"}}
}}
")
                                            
                                            tryCatch({
                                              chat_session <- ellmer::chat_ollama(system_prompt = system_prompt, model = model)
                                              response <- chat_session$chat(user_prompt)
                                              structure_info <- self$parse_llm_response(response)
                                              
                                              if (!is.null(structure_info)) {
                                                message("✓ Document structure analysis completed")
                                              }
                                              
                                              return(structure_info)
                                              
                                            }, error = function(e) {
                                              message(glue("✗ Structure analysis failed: {e$message}"))
                                              return(NULL)
                                            })
                                          },
                                          
                                          parse_llm_response = function(response) {
                                            tryCatch({
                                              # Clean the response
                                              response_clean <- response %>%
                                                str_trim() %>%
                                                str_replace_all("\\n", " ") %>%
                                                str_replace_all("\\s+", " ")
                                              
                                              # Try to extract JSON
                                              json_match <- str_extract(response_clean, "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}")
                                              
                                              if (!is.na(json_match)) {
                                                # Parse JSON and ensure correct structure
                                                structure_info <- jsonlite::fromJSON(json_match, simplifyVector = TRUE)
                                                
                                                # Debug: Print what we got
                                                message("JSON parsed successfully:")
                                                print(str(structure_info))
                                                
                                                # Ensure all regions are numeric vectors, not data.frames
                                                if (!is.null(structure_info$title_regions)) {
                                                  if (is.data.frame(structure_info$title_regions)) {
                                                    structure_info$title_regions <- as.numeric(structure_info$title_regions[[1]])
                                                  } else {
                                                    structure_info$title_regions <- as.numeric(structure_info$title_regions)
                                                  }
                                                }
                                                
                                                if (!is.null(structure_info$author_regions)) {
                                                  if (is.data.frame(structure_info$author_regions)) {
                                                    structure_info$author_regions <- as.numeric(structure_info$author_regions[[1]])
                                                  } else {
                                                    structure_info$author_regions <- as.numeric(structure_info$author_regions)
                                                  }
                                                }
                                                
                                                if (!is.null(structure_info$abstract_regions)) {
                                                  if (is.data.frame(structure_info$abstract_regions)) {
                                                    structure_info$abstract_regions <- as.numeric(structure_info$abstract_regions[[1]])
                                                  } else {
                                                    structure_info$abstract_regions <- as.numeric(structure_info$abstract_regions)
                                                  }
                                                }
                                                
                                                if (!is.null(structure_info$introduction_regions)) {
                                                  if (is.data.frame(structure_info$introduction_regions)) {
                                                    structure_info$introduction_regions <- as.numeric(structure_info$introduction_regions[[1]])
                                                  } else {
                                                    structure_info$introduction_regions <- as.numeric(structure_info$introduction_regions)
                                                  }
                                                }
                                                
                                                return(structure_info)
                                              } else {
                                                message("No valid JSON found, using fallback parsing")
                                                return(self$fallback_parse_response(response))
                                              }
                                            }, error = function(e) {
                                              message(glue("JSON parsing failed: {e$message}"))
                                              message("Using fallback parsing")
                                              return(self$fallback_parse_response(response))
                                            })
                                          },
                                          
                                          fallback_parse_response = function(response) {
                                            title_match <- str_extract(response, "title_regions[\"']*\\s*:\\s*\\[([0-9, ]+)\\]")
                                            author_match <- str_extract(response, "author_regions[\"']*\\s*:\\s*\\[([0-9, ]+)\\]")
                                            abstract_match <- str_extract(response, "abstract_regions[\"']*\\s*:\\s*\\[([0-9, ]+)\\]")
                                            intro_match <- str_extract(response, "introduction_regions[\"']*\\s*:\\s*\\[([0-9, ]+)\\]")
                                            
                                            result <- list(
                                              title_regions = if(!is.na(title_match)) as.numeric(str_extract_all(title_match, "\\d+")[[1]]) else NULL,
                                              author_regions = if(!is.na(author_match)) as.numeric(str_extract_all(author_match, "\\d+")[[1]]) else NULL,
                                              abstract_regions = if(!is.na(abstract_match)) as.numeric(str_extract_all(abstract_match, "\\d+")[[1]]) else NULL,
                                              introduction_regions = if(!is.na(intro_match)) as.numeric(str_extract_all(intro_match, "\\d+")[[1]]) else NULL,
                                              other_regions = list()
                                            )
                                            
                                            return(result)
                                          },
                                          
                                          extract_abstract_raw = function(raw_df, structure_analysis) {
                                            if (is.null(structure_analysis) || is.null(structure_analysis$abstract_regions)) {
                                              # Fallback: find abstract by looking for "abstract" keyword
                                              abstract_candidates <- raw_df %>%
                                                filter(page == 1) %>%
                                                arrange(y, x) %>%
                                                mutate(
                                                  contains_abstract = str_detect(tolower(text), "abstract"),
                                                  is_substantial = nchar(text) > 200,
                                                  row_id = row_number()
                                                )
                                              
                                              # Find the row with "abstract" keyword
                                              abstract_header_row <- abstract_candidates %>%
                                                filter(contains_abstract) %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              if (length(abstract_header_row) > 0) {
                                                # Take substantial text blocks after the abstract header
                                                abstract_text <- abstract_candidates %>%
                                                  filter(row_id > abstract_header_row, is_substantial) %>%
                                                  slice(1:3) %>%  # Take up to 3 blocks after abstract header
                                                  pull(text) %>%
                                                  paste(collapse = " ")
                                              } else {
                                                return("Abstract not found")
                                              }
                                            } else {
                                              abstract_regions <- structure_analysis$abstract_regions
                                              
                                              abstract_text <- raw_df %>%
                                                filter(page == 1) %>%
                                                arrange(y, x) %>%
                                                slice(abstract_regions) %>%
                                                distinct(text, .keep_all = TRUE) %>%
                                                pull(text) %>%
                                                paste(collapse = " ")
                                            }
                                            
                                            # Basic cleaning without LLM
                                            clean_abstract <- abstract_text %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_replace_all("Abstract:?\\s*", "") %>%
                                              str_trim()
                                            
                                            return(clean_abstract)
                                          },
                                          
                                          extract_title_with_llm = function(raw_df, structure_analysis, model) {
                                            # Debug: Check what we received
                                            message("DEBUG: structure_analysis$title_regions:")
                                            print(structure_analysis$title_regions)
                                            print(class(structure_analysis$title_regions))
                                            
                                            if (is.null(structure_analysis) || is.null(structure_analysis$title_regions) || 
                                                length(structure_analysis$title_regions) == 0) {
                                              message("Using fallback title extraction")
                                              title_text <- raw_df %>%
                                                filter(page == 1, y < 600, (w * h) > 50000) %>%
                                                arrange(y, x) %>%
                                                slice(1) %>%
                                                pull(text)
                                            } else {
                                              # Ensure title_regions is numeric
                                              title_regions <- as.numeric(structure_analysis$title_regions)
                                              if (any(is.na(title_regions))) {
                                                message("Invalid title regions, using fallback")
                                                title_text <- raw_df %>%
                                                  filter(page == 1, y < 600, (w * h) > 50000) %>%
                                                  arrange(y, x) %>%
                                                  slice(1) %>%
                                                  pull(text)
                                              } else {
                                                title_text <- raw_df %>%
                                                  filter(page == 1) %>%
                                                  arrange(y, x) %>%
                                                  slice(title_regions[1]) %>%
                                                  pull(text)
                                              }
                                            }
                                            
                                            if (length(title_text) == 0 || nchar(title_text) == 0) return("Title not found")
                                            
                                            system_prompt <- "Clean academic paper titles. Remove duplicates, line breaks, and formatting artifacts. Return only the clean title, no extra text."
                                            
                                            user_prompt <- glue("Clean this title: {title_text}")
                                            
                                            tryCatch({
                                              chat_session <- ellmer::chat_ollama(system_prompt = system_prompt, model = model)
                                              clean_title <- chat_session$chat(user_prompt)
                                              return(str_trim(clean_title))
                                            }, error = function(e) {
                                              return(str_replace_all(title_text, "\\s+", " "))
                                            })
                                          },
                                          
                                          extract_authors_with_llm = function(raw_df, structure_analysis, model) {
                                            if (is.null(structure_analysis) || is.null(structure_analysis$author_regions)) {
                                              authors_text <- raw_df %>%
                                                filter(page == 1, y < 1000, str_detect(text, "\\b[A-Z][a-z]+ [A-Z][a-z]+\\b")) %>%
                                                arrange(y, x) %>%
                                                slice(1) %>%
                                                pull(text)
                                            } else {
                                              authors_text <- raw_df %>%
                                                filter(page == 1) %>%
                                                arrange(y, x) %>%
                                                slice(structure_analysis$author_regions) %>%
                                                pull(text) %>%
                                                paste(collapse = " ")
                                            }
                                            
                                            if (length(authors_text) == 0 || nchar(authors_text) == 0) return("Authors not found")
                                            
                                            system_prompt <- "Extract clean author names from academic papers. Return only the names separated by semicolons. No explanatory text."
                                            
                                            user_prompt <- glue("Extract author names from: {authors_text}")
                                            
                                            tryCatch({
                                              chat_session <- ellmer::chat_ollama(system_prompt = system_prompt, model = model)
                                              clean_authors <- chat_session$chat(user_prompt)
                                              
                                              clean_authors <- clean_authors %>%
                                                str_replace_all("Here.*?:", "") %>%
                                                str_replace_all("The authors.*?:", "") %>%
                                                str_replace_all("Authors.*?:", "") %>%
                                                str_replace_all("^[^A-Z]*", "") %>%
                                                str_trim()
                                              
                                              return(clean_authors)
                                            }, error = function(e) {
                                              return(str_replace_all(authors_text, "\\s+", " "))
                                            })
                                          },
                                          
                                          extract_abstract_with_llm = function(raw_df, structure_analysis, model) {
                                            # First try raw extraction
                                            raw_abstract <- self$extract_abstract_raw(raw_df, structure_analysis)
                                            
                                            if (raw_abstract == "Abstract not found" || nchar(raw_abstract) < 100) {
                                              # Enhanced fallback logic with better error handling
                                              if (is.null(structure_analysis) || is.null(structure_analysis$abstract_regions) ||
                                                  length(structure_analysis$abstract_regions) == 0) {
                                                abstract_candidates <- raw_df %>%
                                                  filter(page == 1, str_detect(tolower(text), "abstract|summary") | nchar(text) > 500) %>%
                                                  arrange(y, x)
                                                
                                                abstract_text <- abstract_candidates %>%
                                                  distinct(text, .keep_all = TRUE) %>%
                                                  pull(text) %>%
                                                  paste(collapse = " ")
                                              } else {
                                                abstract_regions <- as.numeric(structure_analysis$abstract_regions)
                                                if (any(is.na(abstract_regions))) {
                                                  abstract_text <- raw_df %>%
                                                    filter(page == 1, str_detect(tolower(text), "abstract|summary") | nchar(text) > 500) %>%
                                                    arrange(y, x) %>%
                                                    distinct(text, .keep_all = TRUE) %>%
                                                    pull(text) %>%
                                                    paste(collapse = " ")
                                                } else {
                                                  abstract_text <- raw_df %>%
                                                    filter(page == 1) %>%
                                                    arrange(y, x) %>%
                                                    slice(abstract_regions) %>%
                                                    distinct(text, .keep_all = TRUE) %>%
                                                    pull(text) %>%
                                                    paste(collapse = " ")
                                                }
                                              }
                                              
                                              if (nchar(abstract_text) == 0) return("Abstract not found")
                                              
                                              # Very explicit LLM prompt
                                              system_prompt <- "You are a text extraction tool. Extract the exact abstract text without any changes, summaries, or interpretations. Return only the original text with basic formatting cleanup."
                                              
                                              user_prompt <- glue("
Extract the exact abstract text from: {abstract_text}

Rules:
1. Return ONLY the original abstract text
2. Do NOT summarize or rewrite
3. Do NOT add explanatory text
4. Fix only line breaks and spacing

Extracted text:
")
                                              
                                              tryCatch({
                                                chat_session <- ellmer::chat_ollama(system_prompt = system_prompt, model = model)
                                                llm_abstract <- chat_session$chat(user_prompt)
                                                
                                                # If LLM result is much shorter than input, it probably summarized
                                                if (nchar(llm_abstract) < (nchar(abstract_text) * 0.7)) {
                                                  message("⚠ LLM appears to have summarized. Using raw extraction.")
                                                  return(raw_abstract)
                                                }
                                                
                                                # Clean LLM output
                                                clean_abstract <- llm_abstract %>%
                                                  str_replace_all("^(Here is the|The|This is the).*?:?\\s*", "") %>%
                                                  str_replace_all("^Abstract:?\\s*", "") %>%
                                                  str_replace_all("^[^A-Z]*", "") %>%
                                                  str_trim()
                                                
                                                return(clean_abstract)
                                                
                                              }, error = function(e) {
                                                message(glue("✗ LLM abstract extraction failed: {e$message}"))
                                                return(raw_abstract)
                                              })
                                            } else {
                                              # Raw extraction worked well
                                              message("✓ Using raw abstract extraction")
                                              return(raw_abstract)
                                            }
                                          },
                                          
                                          extract_main_text_simple = function(raw_df) {
                                            message("=== DEBUG: Simple main text extraction ===")
                                            
                                            # Step 1: Get all text in order
                                            all_text <- raw_df %>%
                                              filter(nchar(str_trim(text)) > 10) %>%
                                              arrange(page, y, x) %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " "),
                                                row_id = row_number()
                                              )
                                            
                                            message(glue("Total text regions: {nrow(all_text)}"))
                                            
                                            # Step 2: Find references section - look for exact matches
                                            ref_indicators <- all_text %>%
                                              mutate(
                                                is_references = str_detect(str_trim(text), "^References$|^Bibliography$|^Works Cited$|^Literature Cited$"),
                                                is_acknowledgments = str_detect(str_trim(text), "^Acknowledgments$|^Acknowledgements$|^Acknowledgment$"),
                                                is_conflicts = str_detect(str_trim(text), "^Conflicts of Interest$|^Competing Interests$|^Conflict of Interest$"),
                                                is_appendix = str_detect(str_trim(text), "^Appendix|^Supplementary|^Supporting Information")
                                              ) %>%
                                              filter(is_references | is_acknowledgments | is_conflicts | is_appendix) %>%
                                              arrange(row_id)
                                            
                                            if (nrow(ref_indicators) > 0) {
                                              # Find the first occurrence of any end-matter section
                                              cutoff_row <- ref_indicators %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              cutoff_page <- all_text %>%
                                                slice(cutoff_row) %>%
                                                pull(page)
                                              
                                              message(glue("Found end-matter section at row {cutoff_row}, page {cutoff_page}"))
                                              
                                              # Extract everything before the cutoff
                                              main_content <- all_text %>%
                                                slice(1:(cutoff_row - 1)) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                              
                                            } else {
                                              message("No clear end-matter sections found - keeping all content")
                                              
                                              # No clear references section found, keep all content
                                              main_content <- all_text %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                            }
                                            
                                            message(glue("Final main text length: {nchar(main_content)} characters"))
                                            message("=====================================")
                                            
                                            return(main_content)
                                          },
                                          
                                          extract_main_text_smart = function(raw_df) {
                                            message("=== DEBUG: Smart main text extraction ===")
                                            
                                            # Step 1: The text is now properly sorted by your enhanced simple_process_document
                                            # So we can just filter and combine in order
                                            all_text <- raw_df %>%
                                              filter(nchar(str_trim(text)) > 15) %>%
                                              filter(!str_detect(tolower(text), "^(doi|©|page \\d+|figure \\d+|table \\d+|fig\\.|tab\\.)")) %>%
                                              # Text is already in proper reading order from simple_process_document
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " "),
                                                row_id = row_number()
                                              )
                                            
                                            message(glue("Total text regions: {nrow(all_text)}"))
                                            
                                            # Step 2: Find references section
                                            ref_indicators <- all_text %>%
                                              mutate(
                                                is_references = str_detect(str_trim(text), "^References$|^Bibliography$|^Works Cited$|^Literature Cited$"),
                                                is_acknowledgments = str_detect(str_trim(text), "^Acknowledgments$|^Acknowledgements$|^Acknowledgment$"),
                                                is_conflicts = str_detect(str_trim(text), "^Conflicts of Interest$|^Competing Interests$|^Conflict of Interest$"),
                                                is_appendix = str_detect(str_trim(text), "^Appendix|^Supplementary|^Supporting Information")
                                              ) %>%
                                              filter(is_references | is_acknowledgments | is_conflicts | is_appendix) %>%
                                              arrange(row_id)
                                            
                                            if (nrow(ref_indicators) > 0) {
                                              cutoff_row <- ref_indicators %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              cutoff_page <- all_text %>%
                                                slice(cutoff_row) %>%
                                                pull(page)
                                              
                                              message(glue("Found end-matter section at row {cutoff_row}, page {cutoff_page}"))
                                              
                                              # Extract everything before the cutoff
                                              main_content <- all_text %>%
                                                slice(1:(cutoff_row - 1)) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                              
                                            } else {
                                              message("No clear end-matter sections found - keeping all content")
                                              
                                              main_content <- all_text %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                            }
                                            
                                            message(glue("Final main text length: {nchar(main_content)} characters"))
                                            message("=====================================")
                                            
                                            return(main_content)
                                          },
                                          
                                          # New method to extract clean main text (body only)
                                          extract_body_text_only = function(raw_df) {
                                            message("=== DEBUG: Body text extraction (no front matter) ===")
                                            
                                            # Step 1: Text is already properly sorted by column-aware simple_process_document
                                            all_text <- raw_df %>%
                                              filter(nchar(str_trim(text)) > 10) %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " "),
                                                row_id = row_number()
                                              )
                                            
                                            # Step 2: Find where the main body starts
                                            intro_start <- all_text %>%
                                              mutate(
                                                is_intro = str_detect(str_trim(text), "^1\\. Introduction$|^Introduction$|^1\\. Background$|^Background$|^1 Introduction$|^1 Background$"),
                                                is_methods = str_detect(str_trim(text), "^2\\. Methods$|^Methods$|^2\\. Materials and Methods$|^Materials and Methods$|^2 Methods$|^2 Materials and Methods$"),
                                                is_first_section = str_detect(str_trim(text), "^1\\. |^1 ") & nchar(str_trim(text)) < 100
                                              ) %>%
                                              filter(is_intro | is_methods | is_first_section) %>%
                                              arrange(row_id) %>%
                                              slice(1)
                                            
                                            # Step 3: Find where the main body ends
                                            end_matter <- all_text %>%
                                              mutate(
                                                is_references = str_detect(str_trim(text), "^References$|^Bibliography$|^Works Cited$|^Literature Cited$"),
                                                is_acknowledgments = str_detect(str_trim(text), "^Acknowledgments$|^Acknowledgements$|^Acknowledgment$"),
                                                is_conflicts = str_detect(str_trim(text), "^Conflicts of Interest$|^Competing Interests$|^Conflict of Interest$"),
                                                is_appendix = str_detect(str_trim(text), "^Appendix|^Supplementary|^Supporting Information")
                                              ) %>%
                                              filter(is_references | is_acknowledgments | is_conflicts | is_appendix) %>%
                                              arrange(row_id) %>%
                                              slice(1)
                                            
                                            # Extract body text only
                                            if (nrow(intro_start) > 0 && nrow(end_matter) > 0) {
                                              start_row <- intro_start$row_id
                                              end_row <- end_matter$row_id
                                              
                                              message(glue("Body text from row {start_row} to {end_row}"))
                                              
                                              body_content <- all_text %>%
                                                slice(start_row:(end_row - 1)) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                              
                                            } else if (nrow(intro_start) > 0) {
                                              start_row <- intro_start$row_id
                                              
                                              message(glue("Body text from row {start_row} to end (no clear end-matter found)"))
                                              
                                              body_content <- all_text %>%
                                                slice(start_row:nrow(all_text)) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                              
                                            } else {
                                              message("No clear introduction found - using full main text")
                                              body_content <- self$extract_main_text_smart(raw_df)
                                            }
                                            
                                            message(glue("Body text length: {nchar(body_content)} characters"))
                                            message("==============================================")
                                            
                                            return(body_content)
                                          },
                                          
                                          # Enhanced results display
                                          show_results = function(summary) {
                                            cat("=== DOCUMENT PROCESSING RESULTS ===\n")
                                            cat("Title:\n", summary$title, "\n\n")
                                            cat("Authors:\n", summary$authors, "\n\n")
                                            cat("Abstract:\n", str_trunc(summary$abstract, 800), "\n\n")
                                            cat("Total Pages:", summary$total_pages, "\n")
                                            cat("Total Text Regions:", summary$total_text_regions, "\n")
                                            cat("Main Text Length:", nchar(summary$main_text), "characters\n")
                                            cat("===================================\n")
                                          },
                                          
                                          # Method to get clean structured output
                                          get_clean_results = function(summary, prefer_cleaned = TRUE) {
                                            # Choose between cleaned and original versions
                                            if (prefer_cleaned) {
                                              title_final <- if (!is.null(summary$title_cleaned) && nchar(summary$title_cleaned) > 0) {
                                                summary$title_cleaned
                                              } else {
                                                summary$title
                                              }
                                              
                                              abstract_final <- if (!is.null(summary$abstract_cleaned) && nchar(summary$abstract_cleaned) > 0) {
                                                summary$abstract_cleaned
                                              } else {
                                                summary$abstract
                                              }
                                              
                                              main_text_final <- if (!is.null(summary$main_text_cleaned) && nchar(summary$main_text_cleaned) > 0) {
                                                summary$main_text_cleaned
                                              } else {
                                                summary$main_text
                                              }
                                            } else {
                                              title_final <- summary$title
                                              abstract_final <- summary$abstract
                                              main_text_final <- summary$main_text
                                            }
                                            
                                            # Clean authors list (this is always needed regardless of cleaning method)
                                            authors_clean <- if (!is.null(summary$authors)) {
                                              str_split(summary$authors, ";|\\n")[[1]] %>% 
                                                str_trim() %>% 
                                                .[. != ""] %>%
                                                .[!str_detect(., "^(Here|The|Authors)")]
                                            } else {
                                              "Authors not found"
                                            }
                                            
                                            list(
                                              title = title_final,
                                              authors = authors_clean,
                                              abstract = abstract_final,
                                              main_text = main_text_final,
                                              main_text_length = nchar(main_text_final),
                                              metadata = list(
                                                total_pages = summary$total_pages,
                                                total_regions = summary$total_text_regions,
                                                text_length = nchar(main_text_final),
                                                cleaning_applied = !is.null(summary$cleaning_method)
                                              )
                                            )
                                          },
                                          
                                          # Enhanced method to prepare text for Claude analysis
                                          prepare_for_claude = function(summary, body_only = TRUE, 
                                                                        cleaning_method = c("cleaned", "original", "best_available"),
                                                                        include_metadata = TRUE) {
                                            
                                            cleaning_method <- match.arg(cleaning_method)
                                            clean_results <- self$get_clean_results(summary)
                                            
                                            # Determine which text versions to use based on cleaning_method
                                            if (cleaning_method == "cleaned") {
                                              # Use cleaned versions if available, otherwise original
                                              title_text <- if (!is.null(summary$title_cleaned)) summary$title_cleaned else clean_results$title
                                              abstract_text <- if (!is.null(summary$abstract_cleaned)) summary$abstract_cleaned else clean_results$abstract
                                              cleaning_label <- "✓ Cleaned"
                                            } else if (cleaning_method == "original") {
                                              # Always use original versions
                                              title_text <- clean_results$title
                                              abstract_text <- clean_results$abstract
                                              cleaning_label <- "✗ Original"
                                            } else { # "best_available"
                                              # Use the best available version (prefer cleaned, fallback to original)
                                              title_text <- if (!is.null(summary$title_cleaned) && nchar(summary$title_cleaned) > 0) {
                                                summary$title_cleaned
                                              } else {
                                                clean_results$title
                                              }
                                              abstract_text <- if (!is.null(summary$abstract_cleaned) && nchar(summary$abstract_cleaned) > 0) {
                                                summary$abstract_cleaned
                                              } else {
                                                clean_results$abstract
                                              }
                                              cleaning_label <- "✓ Best Available"
                                            }
                                            
                                            # Handle main content - body_only vs full document
                                            if (body_only && !is.null(summary$raw_data)) {
                                              main_content <- self$extract_body_text_only(summary$raw_data)
                                              
                                              # Apply cleaning to body text if requested
                                              if (cleaning_method == "cleaned" && !is.null(summary$main_text_cleaned)) {
                                                # Apply same cleaning method to body text
                                                main_content <- self$apply_cleaning_to_text(main_content, summary$cleaning_method)
                                              } else if (cleaning_method == "best_available") {
                                                # Use best available cleaning for body text
                                                main_content <- self$apply_cleaning_to_text(main_content, summary$cleaning_method)
                                              }
                                              
                                              content_label <- "MAIN CONTENT (Body Only - No Front Matter)"
                                            } else {
                                              # Use full document main text
                                              if (cleaning_method == "cleaned" && !is.null(summary$main_text_cleaned)) {
                                                main_content <- summary$main_text_cleaned
                                              } else if (cleaning_method == "original") {
                                                main_content <- clean_results$main_text
                                              } else { # best_available
                                                main_content <- if (!is.null(summary$main_text_cleaned) && nchar(summary$main_text_cleaned) > 0) {
                                                  summary$main_text_cleaned
                                                } else {
                                                  clean_results$main_text
                                                }
                                              }
                                              content_label <- "MAIN CONTENT (Full Document)"
                                            }
                                            
                                            # Create metadata section if requested
                                            metadata_section <- if (include_metadata) {
                                              glue("
---
Document Statistics:
- Total Pages: {clean_results$metadata$total_pages}
- Main Text Length: {nchar(main_content)} characters
- Text Processing: {cleaning_label}
- Cleaning Method: {if(!is.null(summary$cleaning_method)) summary$cleaning_method else 'Unknown'}
- Column-Aware Processing: ✓ Enabled
- Processing Date: {Sys.Date()}")
                                            } else {
                                              ""
                                            }
                                            
                                            # Create the complete Claude-ready document using glue
                                            claude_text <- glue("
ACADEMIC PAPER ANALYSIS

TITLE: {title_text}

AUTHORS: {paste(clean_results$authors, collapse = '; ')}

ABSTRACT:
{abstract_text}

{content_label}:
{main_content}{metadata_section}
")
                                            
                                            return(claude_text)
                                          },
                                          
                                          # Text cleaning 
                                          clean_text_with_textclean = function(text, academic_mode = TRUE) {
                                            if (!requireNamespace("textclean", quietly = TRUE)) {
                                              stop("textclean package is required for this function")
                                            }
                                            
                                            # Start with textclean's comprehensive cleaning
                                            cleaned <- text %>%
                                              # Remove/replace common textual elements
                                              textclean::replace_contraction() %>%           # Expand contractions
                                              textclean::replace_number(num.paste = TRUE) %>% # Handle numbers consistently
                                              textclean::replace_ordinal(num.paste = TRUE) %>% # Handle ordinals
                                              textclean::replace_symbol() %>%                # Replace symbols with words
                                              textclean::replace_white() %>%                 # Normalize whitespace
                                              textclean::strip() %>%                         # Remove extra whitespace
                                              textclean::replace_non_ascii()                 # Handle non-ASCII characters
                                            
                                            if (academic_mode) {
                                              # Additional academic-specific cleaning
                                              cleaned <- cleaned %>%
                                                # Remove/fix citation patterns (our custom approach)
                                                str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                                str_replace_all('\\s+["\'°*,\\d\\s-]{2,}\\s+(?=[a-zA-Z])', ' ') %>%
                                                # Remove figure/table references
                                                str_replace_all('\\([Ff]ig\\.?\\s*\\d+[^)]*\\)', '') %>%
                                                str_replace_all('\\([Tt]able\\s*\\d+[^)]*\\)', '') %>%
                                                # Fix common academic formatting issues
                                                str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
                                                str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%
                                                # Preserve common scientific notation and units
                                                str_replace_all('(\\d+)\\s*×\\s*10\\s*([−-]?\\d+)', '\\1×10^\\2') %>%
                                                str_replace_all('(\\d+)\\s*mg\\s*L\\s*([−-]?\\d+)', '\\1 mg/L') %>%
                                                str_replace_all('(\\d+)\\s*nm', '\\1 nm') %>%
                                                str_replace_all('p\\s*H', 'pH') %>%
                                                # Clean up whitespace again
                                                str_replace_all('\\s+', ' ') %>%
                                                str_trim()
                                            }
                                            
                                            return(cleaned)
                                          },
                                          
                                          clean_text_with_textclean_academic = function(text) {
                                            if (!requireNamespace("textclean", quietly = TRUE)) {
                                              stop("textclean package is required for this function")
                                            }
                                            
                                            # Step 1: Use selective textclean functions (avoid number conversion)
                                            cleaned <- text %>%
                                              textclean::replace_contraction() %>%           # Expand contractions (can't -> cannot)
                                              textclean::replace_symbol(remove = TRUE) %>%   # Remove symbols rather than convert
                                              textclean::replace_white() %>%                 # Normalize whitespace
                                              textclean::strip() %>%                         # Remove extra whitespace
                                              textclean::replace_non_ascii(remove = TRUE)    # Remove non-ASCII characters
                                            
                                            # Step 2: Academic-specific cleaning (our custom approach)
                                            cleaned <- cleaned %>%
                                              # Remove citation patterns
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{2,}\\s+(?=[a-zA-Z])', ' ') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{3,}\\s+', ' ') %>%
                                              
                                              # Remove figure/table references
                                              str_replace_all('\\([Ff]ig\\.?\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('\\([Tt]able\\s*\\d+[^)]*\\)', '') %>%
                                              
                                              # Fix common academic formatting
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%  # Fix hyphenated words
                                              str_replace_all('\\s+', ' ') %>%                       # Normalize spaces
                                              
                                              # Preserve scientific notation and common units
                                              str_replace_all('(\\d+)\\s*×\\s*10\\s*([−-]?\\d+)', '\\1×10^\\2') %>%
                                              str_replace_all('(\\d+)\\s*nm', '\\1 nm') %>%
                                              str_replace_all('(\\d+)\\s*mg', '\\1 mg') %>%
                                              str_replace_all('p\\s*H', 'pH') %>%

                                              # Final cleanup
                                              str_replace_all('\\s+', ' ') %>%
                                              str_trim()
                                            
                                            return(cleaned)
                                          },
                                          
                                          # Hybrid approach: custom + selective textclean
                                          clean_text_hybrid = function(text) {
                                            # Step 1: Our custom citation and academic cleaning first
                                            cleaned <- text %>%
                                              # Remove citation patterns
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{2,}\\s+(?=[a-zA-Z])', ' ') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{3,}\\s+', ' ') %>%
                                              
                                              # Remove figure/table references
                                              str_replace_all('\\([Ff]ig\\.?\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('\\([Tt]able\\s*\\d+[^)]*\\)', '') %>%
                                              
                                              # Fix hyphenated words
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2')
                                            
                                            # Step 2: Apply selective textclean (avoiding number conversion)
                                            if (requireNamespace("textclean", quietly = TRUE)) {
                                              cleaned <- cleaned %>%
                                                textclean::replace_contraction() %>%
                                                textclean::replace_white() %>%
                                                textclean::strip() %>%
                                                textclean::replace_non_ascii(remove = TRUE)
                                            }
                                            
                                            # Step 3: Final academic-specific fixes
                                            cleaned <- cleaned %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('p\\s*H', 'pH') %>%
                                              str_replace_all('Ag\\s*NPs', 'AgNPs') %>%
                                              str_replace_all('Ag\\s*NP', 'AgNP') %>%
                                              str_trim()
                                            
                                            return(cleaned)
                                          },
                                          
                                          # Enhanced text cleaner with better citation handling
                                          clean_text_for_llm = function(text, aggressive = FALSE) {
                                            if (aggressive) {
                                              return(self$clean_text_for_llm_aggressive(text))
                                            }
                                            
                                            text %>%
                                              # STEP 1: Clean up citation patterns first (most important)
                                              # Remove citations at end of sentences (period followed by citation markers)
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s|$)', '.') %>%
                                              # Remove citations in middle of sentences (common patterns)
                                              str_replace_all('["\'°*,\\d\\s-]{2,}(?=\\s[a-z])', ' ') %>%
                                              str_replace_all('["\'°*,\\d\\s-]{2,}(?=\\s[A-Z])', ' ') %>%
                                              # Remove standalone citation clusters
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{3,}\\s+', ' ') %>%
                                              # Remove citations at start of sentences
                                              str_replace_all('(?<=\\.)\\s*["\'°*,\\d\\s-]+\\s*(?=[A-Z])', ' ') %>%
                                              
                                              # STEP 2: Fix common citation formatting issues
                                              str_replace_all('["""°*]+', '"') %>%
                                              
                                              # STEP 3: Fix spacing issues around concatenated words
                                              str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
                                              
                                              # STEP 4: Clean up multiple spaces (do this after citation removal)
                                              str_replace_all('\\s+', ' ') %>%
                                              
                                              # STEP 5: Remove figure/table references
                                              str_replace_all('\\(Fig\\. \\w+\\)|\\(Table \\w+\\)', '') %>%
                                              str_replace_all('\\(Fig\\.[^)]*\\)|\\(Table[^)]*\\)', '') %>%
                                              
                                              # STEP 6: Fix common broken words (hyphenated across lines)
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%
                                              
                                              # STEP 7: Clean up any remaining citation artifacts
                                              str_replace_all('\\s*["\']{2,}\\s*', ' ') %>%
                                              str_replace_all('\\s*[°*]{1,}\\s*', ' ') %>%
                                              
                                              # STEP 8: Final cleanup
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('\\.\\s*\\.', '.') %>%  # Remove double periods
                                              str_trim()
                                          },
                                          
                                          # More aggressive citation cleaner
                                          clean_text_for_llm_aggressive = function(text) {
                                            text %>%
                                              # STEP 1: Remove all citation-like patterns more aggressively
                                              # Pattern: any combination of quotes, numbers, commas, dashes, periods, spaces
                                              str_replace_all('["\'°*,\\d\\s.-]{3,}(?=\\s|$)', ' ') %>%
                                              
                                              # STEP 2: Clean up common citation patterns specifically
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%  # End of sentence citations
                                              str_replace_all('\\s+["\'°*,\\d\\s-]+\\s+', ' ') %>%          # Mid-sentence citations
                                              
                                              # STEP 3: Standard cleaning
                                              str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('\\(Fig\\.[^)]*\\)|\\(Table[^)]*\\)', '') %>%
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%
                                              str_replace_all('\\.\\s*\\.', '.') %>%
                                              str_trim()
                                          },
                                          
                                          # Method to get both versions
                                          get_text_versions = function(summary) {
                                            clean_results <- self$get_clean_results(summary)
                                            
                                            # Full main text (with front matter)
                                            full_text <- clean_results$main_text
                                            
                                            # Body text only (without front matter)
                                            body_text <- self$extract_body_text_only(summary$raw_data)
                                            
                                            return(list(
                                              full_text = full_text,
                                              body_text = body_text,
                                              full_length = nchar(full_text),
                                              body_length = nchar(body_text),
                                              reduction = nchar(full_text) - nchar(body_text)
                                            ))
                                          },
                                          
                                          # Method to debug text extraction
                                          debug_text_structure = function(raw_df) {
                                            # Show some sample text regions to understand the structure
                                            sample_regions <- raw_df %>%
                                              arrange(page, y, x) %>%
                                              mutate(row_id = row_number()) %>%
                                              filter(nchar(str_trim(text)) > 5) %>%
                                              select(row_id, page, text) %>%
                                              slice(c(1:10, (n()-10):n())) # First 10 and last 10
                                            
                                            print(sample_regions)
                                            
                                            # Look for potential reference sections
                                            ref_candidates <- raw_df %>%
                                              filter(str_detect(str_trim(text), "^References$|^Bibliography$|^Works Cited$|^Literature Cited$|^Acknowledgments$|^Acknowledgements$")) %>%
                                              arrange(page, y, x) %>%
                                              select(page, text)
                                            
                                            cat("\n=== Potential Reference/End Section Headers ===\n")
                                            print(ref_candidates)
                                            
                                            return(list(sample = sample_regions, ref_candidates = ref_candidates))
                                          },
                                          
                                          # Method to validate text quality
                                          validate_text_quality = function(summary) {
                                            # Check for common issues that indicate poor column handling
                                            main_text <- summary$main_text
                                            
                                            issues <- list()
                                            
                                            # Check for broken words (common in poor column handling)
                                            broken_words <- str_count(main_text, "\\w+- \\w+")
                                            if (broken_words > 10) {
                                              issues <- append(issues, glue("High number of hyphenated word breaks: {broken_words}"))
                                            }
                                            
                                            # Check for jumbled citations
                                            jumbled_citations <- str_count(main_text, '["""\'°*]+')
                                            if (jumbled_citations > 20) {
                                              issues <- append(issues, glue("Possible jumbled citations: {jumbled_citations}"))
                                            }
                                            
                                            # Check for sentence fragments (capital letter after period without space)
                                            fragments <- str_count(main_text, "\\.[A-Z]")
                                            if (fragments > 50) {
                                              issues <- append(issues, glue("Possible sentence fragments: {fragments}"))
                                            }
                                            
                                            if (length(issues) == 0) {
                                              message("✓ Text quality looks good!")
                                            } else {
                                              message("⚠ Text quality issues detected:")
                                              for (issue in issues) {
                                                message(glue("  - {issue}"))
                                              }
                                            }
                                            
                                            return(issues)
                                          },
                                          
                                          # Method to compare original vs cleaned text
                                          compare_text_versions = function(summary) {
                                            if (is.null(summary$main_text_cleaned)) {
                                              message("No cleaned version available")
                                              return(NULL)
                                            }
                                            
                                            original_length <- nchar(summary$main_text)
                                            cleaned_length <- nchar(summary$main_text_cleaned)
                                            
                                            # Show some examples of what was cleaned
                                            cat("=== TEXT CLEANING COMPARISON ===\n")
                                            cat("Original length:", original_length, "characters\n")
                                            cat("Cleaned length:", cleaned_length, "characters\n")
                                            cat("Difference:", original_length - cleaned_length, "characters\n")
                                            
                                            # Show a sample of changes (first 1000 characters)
                                            original_sample <- str_sub(summary$main_text, 1, 1000)
                                            cleaned_sample <- str_sub(summary$main_text_cleaned, 1, 1000)
                                            
                                            if (original_sample != cleaned_sample) {
                                              cat("\n=== SAMPLE CHANGES (First 1000 chars) ===\n")
                                              cat("ORIGINAL:\n", original_sample, "\n\n")
                                              cat("CLEANED:\n", cleaned_sample, "\n")
                                            } else {
                                              cat("\n✓ No significant changes in sample text\n")
                                            }
                                            
                                            cat("=====================================\n")
                                          },
                                          
                                          # Add this method to compare different cleaning approaches
                                          compare_cleaning_methods = function(text_sample) {
                                            cat("=== TEXT CLEANING COMPARISON ===\n\n")
                                            
                                            # Original
                                            cat("1. ORIGINAL (first 500 chars):\n")
                                            cat(str_sub(text_sample, 1, 500), "\n\n")
                                            
                                            # Our custom aggressive
                                            if (exists("clean_text_for_llm_aggressive")) {
                                              aggressive_clean <- self$clean_text_for_llm_aggressive(text_sample)
                                              cat("2. CUSTOM AGGRESSIVE (first 500 chars):\n")
                                              cat(str_sub(aggressive_clean, 1, 500), "\n\n")
                                            }
                                            
                                            # textclean basic
                                            if (requireNamespace("textclean", quietly = TRUE)) {
                                              textclean_basic <- self$clean_text_with_textclean(text_sample, academic_mode = FALSE)
                                              cat("3. TEXTCLEAN BASIC (first 500 chars):\n")
                                              cat(str_sub(textclean_basic, 1, 500), "\n\n")
                                              
                                              # textclean academic
                                              textclean_academic <- self$clean_text_with_textclean(text_sample, academic_mode = TRUE)
                                              cat("4. TEXTCLEAN ACADEMIC (first 500 chars):\n")
                                              cat(str_sub(textclean_academic, 1, 500), "\n\n")
                                            }
                                            
                                            # Length comparison
                                            cat("LENGTH COMPARISON:\n")
                                            cat("Original:", nchar(text_sample), "characters\n")
                                            if (exists("aggressive_clean")) cat("Custom Aggressive:", nchar(aggressive_clean), "characters\n")
                                            if (exists("textclean_basic")) cat("Textclean Basic:", nchar(textclean_basic), "characters\n")
                                            if (exists("textclean_academic")) cat("Textclean Academic:", nchar(textclean_academic), "characters\n")
                                            
                                            cat("=====================================\n")
                                          },
                                          
                                          # Helper method to apply cleaning to any text based on the method used
                                          apply_cleaning_to_text = function(text, cleaning_method) {
                                            if (is.null(cleaning_method)) return(text)
                                            
                                            switch(cleaning_method,
                                                   "custom_aggressive" = self$clean_text_for_llm_aggressive(text),
                                                   "textclean_academic" = self$clean_text_with_textclean_academic(text),
                                                   "hybrid" = self$clean_text_hybrid(text),
                                                   text  # fallback to original
                                            )
                                          }
                                        )
)

# ==== HELPER FUNCTIONS =====
simple_process_document <- function(pdf_path) {
  message(glue("Processing PDF: {pdf_path}"))
  
  # Setup
  use_condaenv("PDF_ocr")
  cv2 <- import("cv2")
  dlyolo <- import("doclayout_yolo")
  YOLOv10 <- dlyolo$YOLOv10
  
  model <- YOLOv10("/home/william-ackerman/Desktop/MINER_DB/doclayout_yolo_docstructbench_imgsz1024.pt")
  
  # Convert PDF to images
  pdf_images <- pdf_convert(pdf_path, dpi = 300)
  message(glue("✓ Converted {length(pdf_images)} pages to images"))
  
  df_container <- list()
  
  for(i in 1:length(pdf_images)) {
    pdf_image <- pdf_images[i]
    page_num <- i
    message(glue("Processing page {page_num}"))
    
    tryCatch({
      # YOLO prediction
      det_res <- model$predict(
        pdf_image,
        imgsz = as.integer(1024),
        conf = 0.2,
        device = "cuda:0"
      )
      
      # Explicitly set variables in Python environment
      py$det_res <- det_res
      py$cv2 <- cv2
      py$pdf_image <- pdf_image
      
      # Extract bounding boxes
      py_run_string("
try:
    bounding_boxes = det_res[0].summary()
    print(f'Found {len(bounding_boxes)} bounding boxes')
except Exception as e:
    print(f'Error extracting bounding boxes: {e}')
    bounding_boxes = []
      ")
      
      # Save annotated image
      img_name <- str_split(pdf_image, "\\.")[[1]][1]
      res_name <- paste0("prediction_", img_name, ".jpg")
      py$res_name <- res_name
      
      py_run_string("
try:
    annotated_frame = det_res[0].plot(pil=True, line_width=5, font_size=20)
    cv2.imwrite(res_name, annotated_frame)
    print('Annotated image saved')
except Exception as e:
    print(f'Error saving annotated image: {e}')
      ")
      
      bounding_boxes <- py$bounding_boxes
      
      if (length(bounding_boxes) == 0) {
        message("No bounding boxes found")
        next
      }
      
      # Filter for target elements
      filtered_items <- keep(bounding_boxes, ~.x$name %in% c("plain text", "title"))
      
      if (length(filtered_items) == 0) {
        message("No target elements found")
        next
      }
      
      # Convert to boxes format
      boxes <- map(filtered_items, ~{
        list(
          floor(.x$box$x1) %>% as.integer(),
          floor(.x$box$y1) %>% as.integer(),
          floor(.x$box$x2 - .x$box$x1) %>% as.integer(),
          floor(.x$box$y2 - .x$box$y1) %>% as.integer()
        )
      })
      
      # Set up for OCR
      py$boxes <- boxes
      
      py_run_string("
import cv2
import pytesseract

image = cv2.imread(pdf_image)
results = []

for (x, y, w, h) in boxes:
    try:
        roi = image[y:y+h, x:x+w]
        gray_roi = cv2.cvtColor(roi, cv2.COLOR_BGR2GRAY)
        text = pytesseract.image_to_string(gray_roi)
        
        results.append({
            'bounding_box': (x, y, w, h),
            'text': text
        })
    except Exception as e:
        print(f'Error processing box: {e}')
        continue
      ")
      
      # Convert to dataframe
      ocr_results <- py$results
      
      if (length(ocr_results) > 0) {
        df <- map_dfr(ocr_results, ~{
          bbox <- .x$bounding_box
          tibble(
            page = page_num,
            x = bbox[[1]],
            y = bbox[[2]],
            w = bbox[[3]],
            h = bbox[[4]],
            text = .x$text
          )
        })
        
        df <- df %>% 
          filter(nchar(trimws(text)) > 0)
        
        # **NEW: Smart column-aware sorting**
        df <- sort_by_reading_order(df, page_num)
        
        df_container[[i]] <- df
        message(glue("✓ Page {page_num} processed successfully"))
      }
      
    }, error = function(e) {
      message(glue("✗ Error processing page {page_num}: {e$message}"))
    })
  }
  
  # Combine results
  if (length(df_container) > 0) {
    complete_df <- bind_rows(df_container)
    
    # Save results
    output_file <- glue("{pdf_path}.RData")
    save(complete_df, file = output_file)
    message(glue("✓ Results saved to {output_file}"))
    
    return(complete_df)
  } else {
    message("No pages processed successfully")
    return(NULL)
  }
}

# Smart reading order function - this is the key enhancement
sort_by_reading_order <- function(df, page_num) {
  if (nrow(df) == 0) return(df)
  
  message(glue("  Analyzing reading order for page {page_num}"))
  
  # Calculate center points and dimensions - INCLUDING AREA
  df <- df %>%
    mutate(
      x_center = x + w/2,
      y_center = y + h/2,
      x_right = x + w,
      area = w * h  # CALCULATE AREA HERE
    )
  
  # Step 1: Detect page layout
  layout_info <- detect_page_layout(df)
  
  if (layout_info$columns == 1) {
    # Single column: simple top-to-bottom sorting
    message("  → Single column sorting")
    result <- df %>%
      arrange(y, x) %>%
      select(-x_center, -y_center, -x_right, -area)
    
  } else if (layout_info$columns == 2) {
    # Two columns: sort within each column, then combine
    message(glue("  → Two columns sorting (boundary at x={round(layout_info$boundary)})"))
    
    result <- df %>%
      mutate(
        column = ifelse(x_center < layout_info$boundary, 1, 2)
      ) %>%
      # Sort within each column by y-position
      arrange(column, y, x) %>%
      select(-x_center, -y_center, -x_right, -area, -column)
  }
  
  return(result)
}

# Enhanced layout detection with academic paper heuristics
detect_page_layout <- function(df) {
  x_positions <- df$x_center
  areas <- df$area  # Now calculated in sort_by_reading_order
  
  if (length(x_positions) < 2) {
    return(list(columns = 1, boundary = NULL, boundaries = NULL))
  }
  
  # Academic paper column detection with known patterns
  # Based on your analysis: Column 1 ~690-692, Column 2 ~1788-1790
  
  # Filter out likely outliers (very small areas - headers, page numbers)
  main_text_threshold <- quantile(areas, 0.2)  # Bottom 20% likely metadata
  
  main_text_boxes <- df %>%
    filter(area >= main_text_threshold) %>%
    pull(x_center)
  
  if (length(main_text_boxes) < 2) {
    # Not enough main text - probably a figure page or references
    return(list(columns = 1, boundary = NULL, boundaries = NULL))
  }
  
  # Look for the classic academic paper column positions
  left_column_positions <- main_text_boxes[main_text_boxes < 1000]   # Left column ~690-692
  right_column_positions <- main_text_boxes[main_text_boxes > 1500]  # Right column ~1788-1790
  
  # If we have substantial content in both regions, it's 2-column
  if (length(left_column_positions) >= 1 && length(right_column_positions) >= 1) {
    
    # Calculate boundary as midpoint between typical column positions
    # Use median positions to be robust against outliers
    left_median <- median(left_column_positions)
    right_median <- median(right_column_positions)
    boundary <- (left_median + right_median) / 2
    
    # Ensure boundary is reasonable (between 1200-1400 based on your data)
    boundary <- max(1200, min(1400, boundary))
    
    gap <- right_median - left_median
    message(glue("  → Two columns: Left (~{round(left_median)}), Right (~{round(right_median)}), Gap: {round(gap)}px"))
    return(list(columns = 2, boundary = boundary, boundaries = c(boundary)))
  }
  
  # Special case: References pages (pages 20-22)
  # These have content in both columns but are usually lists
  if (length(main_text_boxes) >= 2) {
    x_sorted <- sort(main_text_boxes)
    max_gap <- max(diff(x_sorted))
    
    # If there's a big gap (>800px) and positions look like our columns
    if (max_gap > 800) {
      left_pos <- x_sorted[x_sorted < mean(x_sorted)]
      right_pos <- x_sorted[x_sorted >= mean(x_sorted)]
      
      if (length(left_pos) >= 1 && length(right_pos) >= 1) {
        boundary <- (max(left_pos) + min(right_pos)) / 2
        message(glue("  → Two columns (references): Left (~{round(median(left_pos))}), Right (~{round(median(right_pos))}), Gap: {round(max_gap)}px"))
        return(list(columns = 2, boundary = boundary, boundaries = c(boundary)))
      }
    }
  }
  
  # Default to single column
  message("  → Single column (or special layout)")
  return(list(columns = 1, boundary = NULL, boundaries = NULL))
}




# ==== RUN THE CODE ====
setwd("/home/william-ackerman/Desktop/TEST_FOLDER_2")
list.files

pdf <- "Inhibition of bacterial adhesion and biofilm formation by dual functional textured and nitric oxide releasing surfaces.pdf"

# Process with improved column handling
processor <- GenericDocumentProcessor$new()

# Use textclean with academic mode
summary <- processor$process_document(pdf, cleaning_method = "hybrid")

# Use cleaned versions (default)
claude_text_cleaned <- processor$prepare_for_claude(summary, body_only = TRUE, cleaning_method = "cleaned")

# Use original versions
claude_text_original <- processor$prepare_for_claude(summary, body_only = TRUE, cleaning_method = "original")

# Use best available (cleaned if good, original if not)
claude_text_best <- processor$prepare_for_claude(summary, body_only = TRUE, cleaning_method = "best_available")

# Without metadata for shorter output
claude_text_minimal <- processor$prepare_for_claude(summary, body_only = TRUE, include_metadata = FALSE)

# Compare different approaches
processor$compare_text_versions(summary)
