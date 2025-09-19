# 1. PACKAGES AND SETUP
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

# 2. MAIN DOCUMENT PROCESSOR (Simplified)
GenericDocumentProcessor <- R6::R6Class("GenericDocumentProcessor",
                                        
                                        private = list(
                                          main_text_original = NULL,
                                          main_text_body_only = NULL
                                        ),
                                        
                                        public = list(
                                          
                                          detect_document_format = function(raw_df) {
                                            first_page <- raw_df %>% filter(page == 1)
                                            all_text <- paste(tolower(first_page$text), collapse = " ")
                                            
                                            format_scores <- list(
                                              nature_communications = 0,
                                              rsc = 0,
                                              elsevier = 0,
                                              mdpi = 0,
                                              generic_academic = 1
                                            )
                                            
                                            # Check abandon elements AND plain text for publisher info
                                            publisher_text <- if ("element_type" %in% colnames(first_page)) {
                                              first_page %>%
                                                filter(element_type %in% c("abandon", "plain text")) %>%
                                                pull(text) %>%
                                                paste(collapse = " ") %>%
                                                tolower()
                                            } else {
                                              all_text
                                            }

                                            # MDPI detection (enhanced)
                                            if (str_detect(publisher_text, "mdpi|biomolecules|nanomaterials|materials|sustainability|applied sciences")) {
                                              format_scores$mdpi <- format_scores$mdpi + 5
                                            }
                                            if (str_detect(publisher_text, "licensee mdpi|basel, switzerland")) {
                                              format_scores$mdpi <- format_scores$mdpi + 3
                                            }
                                            if (str_detect(publisher_text, "academic editor:|received:.*revised:.*accepted:.*published:")) {
                                              format_scores$mdpi <- format_scores$mdpi + 2
                                            }
                                            
                                            # Nature detection (more patterns)
                                            if (str_detect(publisher_text, "nature.*communications|doi.*nature|nature\\.com")) {
                                              format_scores$nature_communications <- format_scores$nature_communications + 3
                                            }
                                            
                                            # RSC detection
                                            if (str_detect(publisher_text, "royal society|environmental science|rsc\\.li|rsc publishing")) {
                                              format_scores$rsc <- format_scores$rsc + 3
                                            }
                                            
                                            # Elsevier detection
                                            if (str_detect(publisher_text, "elsevier|sciencedirect")) {
                                              format_scores$elsevier <- format_scores$elsevier + 3
                                            }
                                            
                                            # Layout-based hints
                                            title_regions <- first_page %>%
                                              filter(y < 600, nchar(text) > 30) %>%
                                              arrange(y)
                                            
                                            if (nrow(title_regions) > 0) {
                                              title_y <- title_regions$y[1]
                                              # Nature typically has titles higher up
                                              if (title_y < 300) format_scores$nature_communications <- format_scores$nature_communications + 1
                                              # RSC often has more compact layouts
                                              if (title_y > 200 & title_y < 400) format_scores$rsc <- format_scores$rsc + 1
                                            }
                                            
                                            # Return format with highest score
                                            best_format <- names(format_scores)[which.max(format_scores)]
                                            message(glue("✓ Detected: {best_format} (score: {max(unlist(format_scores))})"))
                                            return(best_format)
                                          },
                                          
                                          # MAIN METHOD 
                                          process_document = function(pdf_path, model = "llama3.1", 
                                                                      cleaning_method = c("custom_aggressive", 
                                                                                          "textclean_academic", 
                                                                                          "hybrid", "none")) {
                                            
                                            cleaning_method <- match.arg(cleaning_method)
                                            
                                            # Extract raw text
                                            raw_df <- simple_process_document(pdf_path)
                                            if (is.null(raw_df)) {
                                              message("Failed to extract raw text")
                                              return(NULL)
                                            }
                                            
                                            # Detect format
                                            doc_format <- self$detect_document_format(raw_df)
                                            
                                            # Extract content using LLM
                                            first_page_regions <- self$prepare_regions_for_analysis(raw_df)
                                            structure_analysis <- self$analyze_document_structure(first_page_regions, model)
                                            
                                            # Extract individual components
                                            title <- self$extract_title_universal(raw_df, structure_analysis, model)
                                            authors <- self$extract_authors_with_llm(raw_df, structure_analysis, model)
                                            abstract <- self$extract_abstract_universal(raw_df, structure_analysis, model)
                                            
                                            # Extract main text using abstract as boundary marker
                                            main_text_result <- self$extract_main_text_smart(raw_df, abstract)
                                            main_text <- main_text_result$main
                                            
                                            # Apply cleaning
                                            if (cleaning_method != "none") {
                                              main_text_cleaned <- self$apply_cleaning_method(main_text, cleaning_method)
                                              title_cleaned <- self$apply_cleaning_method(title, cleaning_method)
                                              abstract_cleaned <- self$apply_cleaning_method(abstract, cleaning_method)
                                            } else {
                                              main_text_cleaned <- main_text
                                              title_cleaned <- title
                                              abstract_cleaned <- abstract
                                            }
                                            
                                            # Create enhanced summary
                                            summary <- list(
                                              title = title,
                                              title_cleaned = title_cleaned,
                                              authors = authors,
                                              abstract = abstract,
                                              abstract_cleaned = abstract_cleaned,
                                              main_text = main_text,  # Clean body-only version
                                              main_text_cleaned = main_text_cleaned,
                                              main_text_original = main_text_result$original,  # Full original version
                                              extraction_boundaries = main_text_result$boundaries,  # For debugging
                                              document_format = doc_format,
                                              total_pages = max(raw_df$page),
                                              total_text_regions = nrow(raw_df),
                                              raw_data = raw_df,
                                              cleaning_method = cleaning_method
                                            )
                                            
                                            # Save results
                                            output_file <- glue("{pdf_path}_llm_processed.RData")
                                            save(summary, file = output_file)
                                            message(glue("✓ Results saved to {output_file}"))
                                            
                                            return(summary)
                                          },
                                          
                                          process_document_simple = function(pdf_path, 
                                                                             cleaning_method = c("custom_aggressive", 
                                                                                                 "textclean_academic", 
                                                                                                 "hybrid", "none")) {
                                            
                                            cleaning_method <- match.arg(cleaning_method)
                                            
                                            # Extract raw text
                                            raw_df <- simple_process_document(pdf_path)
                                            if (is.null(raw_df)) {
                                              message("Failed to extract raw text")
                                              return(NULL)
                                            }
                                            
                                            # Use simple universal extraction
                                            simple_results <- self$extract_content_simple_universal(raw_df)
                                            
                                            # Minor cleanup of results
                                            title_clean <- simple_results$title %>%
                                              str_replace_all("lonic-?\\s*", "Ionic ") %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            authors_clean <- simple_results$authors %>%
                                              str_replace_all("[°*†‡§¶#¹²³⁴⁵⁶⁷⁸⁹⁰'\"?,]", "") %>%  # Remove superscripts
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            abstract_clean <- simple_results$abstract %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            main_content_clean <- simple_results$main_content
                                            
                                            # Apply cleaning if requested
                                            if (cleaning_method != "none") {
                                              title_clean <- self$apply_cleaning_method(title_clean, cleaning_method)
                                              authors_clean <- self$apply_cleaning_method(authors_clean, cleaning_method)  
                                              abstract_clean <- self$apply_cleaning_method(abstract_clean, cleaning_method)
                                              main_content_clean <- self$apply_cleaning_method(main_content_clean, cleaning_method)
                                            }
                                            
                                            # Create summary
                                            summary <- list(
                                              title = simple_results$title,
                                              title_cleaned = title_clean,
                                              authors = authors_clean,  # Already cleaned
                                              abstract = simple_results$abstract,
                                              abstract_cleaned = abstract_clean,
                                              main_text = main_content_clean,  # This is the body-only version
                                              main_text_cleaned = main_content_clean,  # Same as above after cleaning
                                              main_text_original = paste(raw_df$text, collapse = " "),  # Full original
                                              extraction_method = "simple_universal",
                                              document_format = "auto_detected",
                                              total_pages = max(raw_df$page),
                                              total_text_regions = nrow(raw_df),
                                              raw_data = raw_df,
                                              cleaning_method = cleaning_method
                                            )
                                            
                                            # Save results
                                            output_file <- glue("{pdf_path}_simple_processed.RData")
                                            save(summary, file = output_file)
                                            message(glue("✓ Results saved to {output_file}"))
                                            
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
                                                  # Improve editor detection to avoid confusion with authors
                                                  str_detect(text, "^Editor:|^Edited by:|^Handling Editor:") ~ "editor_info",
                                                  
                                                  # Better author detection - look for multiple names with special characters
                                                  str_detect(text, "[°*†‡§¶#¹²³⁴⁵⁶⁷⁸⁹⁰]") & 
                                                    str_count(text, "[A-Z][a-z]+\\s+[A-Z][a-z]+") >= 2 ~ "likely_author",
                                                  
                                                  # Title detection
                                                  y < 600 & nchar(text) > 50 & nchar(text) < 300 & 
                                                    element_type == "title" ~ "likely_title",
                                                  
                                                  # Abstract detection
                                                  str_detect(tolower(text), "abstract|summary") | 
                                                    (nchar(text) > 500 & y > 600 & y < 2000) ~ "likely_abstract",
                                                  
                                                  # Other patterns
                                                  str_detect(tolower(text), "^1\\.|introduction") ~ "likely_introduction",
                                                  str_detect(tolower(text), "environmental significance") ~ "likely_env_sig",
                                                  str_detect(text, "doi|cite|received|accepted|rsc\\.li") ~ "likely_citation",
                                                  str_detect(text, "university|department|institute|school") & 
                                                    !str_detect(text, "[°*†‡§¶#]") ~ "likely_affiliations",
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
                                              # More aggressive JSON extraction
                                              response_clean <- response %>% str_trim()
                                              
                                              # Find JSON boundaries more precisely
                                              json_start <- str_locate(response_clean, "\\{")[1,1]
                                              
                                              if (!is.na(json_start)) {
                                                # Find the matching closing brace
                                                json_part <- str_sub(response_clean, json_start)
                                                
                                                # Count braces to find where JSON actually ends
                                                brace_count <- 0
                                                json_end <- 1
                                                
                                                for (i in 1:nchar(json_part)) {
                                                  char <- str_sub(json_part, i, i)
                                                  if (char == "{") brace_count <- brace_count + 1
                                                  if (char == "}") brace_count <- brace_count - 1
                                                  
                                                  if (brace_count == 0) {
                                                    json_end <- i
                                                    break
                                                  }
                                                }
                                                
                                                # Extract only the complete JSON part
                                                clean_json <- str_sub(json_part, 1, json_end)
                                                message(glue("Extracted JSON: {str_trunc(clean_json, 100)}"))
                                                
                                                # Parse the clean JSON
                                                structure_info <- jsonlite::fromJSON(clean_json, simplifyVector = TRUE)
                                                
                                                # Clean up the structure
                                                if (!is.null(structure_info$title_regions)) {
                                                  structure_info$title_regions <- as.numeric(structure_info$title_regions)
                                                }
                                                if (!is.null(structure_info$author_regions)) {
                                                  structure_info$author_regions <- as.numeric(structure_info$author_regions)
                                                }
                                                if (!is.null(structure_info$abstract_regions)) {
                                                  structure_info$abstract_regions <- as.numeric(structure_info$abstract_regions)
                                                }
                                                if (!is.null(structure_info$introduction_regions)) {
                                                  structure_info$introduction_regions <- as.numeric(structure_info$introduction_regions)
                                                }
                                                
                                                return(structure_info)
                                              }
                                            }, error = function(e) {
                                              message(glue("JSON parsing failed: {e$message}"))
                                            })
                                            
                                            # Enhanced fallback parsing
                                            message("Using enhanced fallback parsing")
                                            return(self$enhanced_fallback_parse_response(response))
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
                                          
                                          enhanced_fallback_parse_response = function(response) {
                                            message("=== Fallback Response Parsing ===")
                                            message(glue("Full response: {str_trunc(response, 200)}"))
                                            
                                            # More robust regex-based parsing
                                            result <- list(
                                              title_regions = NULL,
                                              author_regions = NULL,
                                              abstract_regions = NULL,
                                              introduction_regions = NULL,
                                              other_regions = list()
                                            )
                                            
                                            # Look for arrays with better patterns
                                            title_nums <- str_extract_all(response, "title_regions[^\\[]*\\[([0-9,\\s]+)\\]")[[1]]
                                            if (length(title_nums) > 0) {
                                              numbers <- str_extract_all(title_nums[1], "\\d+")[[1]]
                                              if (length(numbers) > 0) {
                                                result$title_regions <- as.numeric(numbers)
                                                message(glue("Found title regions: {paste(result$title_regions, collapse=', ')}"))
                                              }
                                            }
                                            
                                            author_nums <- str_extract_all(response, "author_regions[^\\[]*\\[([0-9,\\s]+)\\]")[[1]]
                                            if (length(author_nums) > 0) {
                                              numbers <- str_extract_all(author_nums[1], "\\d+")[[1]]
                                              if (length(numbers) > 0) {
                                                result$author_regions <- as.numeric(numbers)
                                                message(glue("Found author regions: {paste(result$author_regions, collapse=', ')}"))
                                              }
                                            }
                                            
                                            abstract_nums <- str_extract_all(response, "abstract_regions[^\\[]*\\[([0-9,\\s]+)\\]")[[1]]
                                            if (length(abstract_nums) > 0) {
                                              numbers <- str_extract_all(abstract_nums[1], "\\d+")[[1]]
                                              if (length(numbers) > 0) {
                                                result$abstract_regions <- as.numeric(numbers)
                                                message(glue("Found abstract regions: {paste(result$abstract_regions, collapse=', ')}"))
                                              }
                                            }
                                            
                                            # If we found nothing, make reasonable assumptions
                                            if (is.null(result$title_regions) && is.null(result$author_regions) && is.null(result$abstract_regions)) {
                                              message("No regions found in response, using defaults")
                                              result$title_regions <- c(1)
                                              result$author_regions <- c(2)
                                              result$abstract_regions <- c(3, 4, 5)  # More regions for abstract
                                            }
                                            
                                            return(result)
                                          },
                                          
                                          extract_title_with_llm = function(raw_df, structure_analysis, model) {
                                            return(self$extract_title_universal(raw_df, structure_analysis, model))
                                          },
                                          
                                          extract_authors_with_llm = function(raw_df, structure_analysis, model) {
                                            message("DEBUG: Enhanced author extraction...")
                                            
                                            # Apply proper reading order first
                                            page1_sorted <- raw_df %>%
                                              filter(page == 1) %>%
                                              mutate(
                                                x_center = x + w/2,
                                                y_center = y + h/2,
                                                area = w * h
                                              )
                                            
                                            # Detect format for targeted author extraction
                                            is_jsm_format <- any(str_detect(tolower(page1_sorted$text), "jsm nanotechnology|scimed.*central"))
                                            is_nature_format <- any(str_detect(tolower(page1_sorted$text), "nature.*nanotechnology"))
                                            
                                            # Apply reading order
                                            layout_info <- detect_page_layout_universal(page1_sorted)
                                            if (layout_info$columns == 2) {
                                              page1_sorted <- page1_sorted %>%
                                                mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
                                                arrange(column, y, x) %>%
                                                select(-x_center, -y_center, -area, -column)
                                            } else {
                                              page1_sorted <- page1_sorted %>%
                                                arrange(y, x) %>%
                                                select(-x_center, -y_center, -area)
                                            }
                                            
                                            # Format-specific author detection
                                            if (is_jsm_format) {
                                              message("Using JSM format author detection")
                                              
                                              # JSM format has authors in a specific area with clear patterns
                                              jsm_author_candidates <- page1_sorted %>%
                                                filter(
                                                  y >= 600 & y < 1200,  # Author area in JSM format
                                                  (str_detect(text, "Mehrdad.*Tajkarimi|Kristen.*Rhinehardt") |
                                                     str_count(text, "[A-Z][a-z]+\\s+[A-Z][a-z]+") >= 3 |
                                                     str_detect(text, "\\d+,\\d+") & nchar(text) > 50)  # Superscript affiliations
                                                ) %>%
                                                arrange(y)
                                              
                                              if (nrow(jsm_author_candidates) > 0) {
                                                message("JSM author candidates found:")
                                                jsm_author_candidates %>%
                                                  mutate(preview = str_trunc(text, 80)) %>%
                                                  select(y, preview) %>%
                                                  print()
                                                
                                                authors_text <- jsm_author_candidates %>%
                                                  pull(text) %>%
                                                  paste(collapse = " ")
                                              } else {
                                                message("No JSM author candidates found, using fallback")
                                                authors_text <- "Authors not found"
                                              }
                                              
                                            } else if (is_nature_format) {
                                              # Nature format author detection
                                              nature_author_candidates <- page1_sorted %>%
                                                filter(
                                                  y >= 400 & y < 600,
                                                  str_detect(text, "([A-Z][a-z]+.*){2,}") & str_detect(text, "\\d+")
                                                ) %>%
                                                arrange(y)
                                              
                                              if (nrow(nature_author_candidates) > 0) {
                                                authors_text <- nature_author_candidates %>%
                                                  pull(text) %>%
                                                  paste(collapse = " ")
                                              } else {
                                                authors_text <- "Authors not found"
                                              }
                                              
                                            } else {
                                              # Generic author detection (existing logic)
                                              author_candidates <- page1_sorted %>%
                                                filter(!str_detect(text, "^Editor:|^Edited by:|^Handling Editor:")) %>%
                                                mutate(
                                                  name_count = str_count(text, "[A-Z][a-z]+\\s+[A-Z][a-z]+"),
                                                  has_multiple_names = name_count >= 2,
                                                  has_superscripts = str_detect(text, "[°*†‡§¶#¹²³⁴⁵⁶⁷⁸⁹⁰]"),
                                                  has_commas = str_count(text, ",") >= 1,
                                                  is_reasonable_length = nchar(text) > 30 & nchar(text) < 1000,
                                                  
                                                  author_score = as.numeric(has_multiple_names) * 4 +
                                                    as.numeric(has_superscripts) * 3 +
                                                    as.numeric(has_commas) * 2 +
                                                    as.numeric(is_reasonable_length) * 1
                                                ) %>%
                                                filter(author_score >= 5) %>%
                                                arrange(desc(author_score), y)
                                              
                                              if (nrow(author_candidates) > 0) {
                                                authors_text <- author_candidates %>%
                                                  slice(1) %>%
                                                  pull(text)
                                              } else {
                                                authors_text <- "Authors not found"
                                              }
                                            }
                                            
                                            message(glue("Raw authors text: {str_trunc(authors_text, 150)}"))
                                            
                                            # Clean up with LLM if we found something
                                            if (!str_detect(authors_text, "Authors not found") && nchar(authors_text) > 10) {
                                              
                                              # Enhanced system prompt for better author extraction
                                              system_prompt <- "Extract ONLY the author names from academic paper text. Remove all affiliations, numbers, symbols, institutional information, and email addresses. Return names separated by semicolons. If multiple similar names appear, include only unique names."
                                              
                                              user_prompt <- glue("Extract author names from this text: {authors_text}")
                                              
                                              tryCatch({
                                                chat_session <- ellmer::chat_ollama(system_prompt = system_prompt, model = model)
                                                llm_authors <- chat_session$chat(user_prompt)
                                                
                                                # Clean LLM output more aggressively
                                                llm_authors <- llm_authors %>%
                                                  str_replace_all("^(Here.*:|The authors.*:|Authors.*:|Author names.*:)\\s*", "") %>%
                                                  str_replace_all("\\d+", "") %>%
                                                  str_replace_all("[\\(\\)\\[\\]@\\*°†‡§¶#]", "") %>%
                                                  str_replace_all("\\s+", " ") %>%
                                                  str_replace_all(",\\s*,", ",") %>%  # Fix double commas
                                                  str_replace_all(";\\s*;", ";") %>%  # Fix double semicolons
                                                  str_trim()
                                                
                                                # Verify we got reasonable names
                                                if (nchar(llm_authors) > 5 && 
                                                    str_detect(llm_authors, "[A-Z][a-z]+") &&
                                                    !str_detect(tolower(llm_authors), "sorry|cannot|unable|editor")) {
                                                  message(glue("Extracted authors (LLM): {llm_authors}"))
                                                  return(llm_authors)
                                                } else {
                                                  message("LLM output suspicious, using cleaned original")
                                                  clean_authors <- authors_text %>%
                                                    str_replace_all("[°*†‡§¶#¹²³⁴⁵⁶⁷⁸⁹⁰\"»]", "") %>%
                                                    str_replace_all("\\s+", " ") %>%
                                                    str_trim()
                                                  return(clean_authors)
                                                }
                                                
                                              }, error = function(e) {
                                                message("LLM extraction failed, using basic cleanup")
                                                clean_authors <- authors_text %>%
                                                  str_replace_all("[°*†‡§¶#¹²³⁴⁵⁶⁷⁸⁹⁰\"»]", "") %>%
                                                  str_replace_all("\\s+", " ") %>%
                                                  str_trim()
                                                return(clean_authors)
                                              })
                                            }
                                            
                                            return("Authors not found")
                                          },
                                          
                                          extract_abstract_with_llm = function(raw_df, structure_analysis, model) {
                                            
                                            message("DEBUG: Looking for abstract...")
                                            
                                            # Check if LLM found abstract regions - but be more permissive
                                            if (!is.null(structure_analysis) && !is.null(structure_analysis$abstract_regions) && 
                                                length(structure_analysis$abstract_regions) > 0) {
                                              
                                              message(glue("Using LLM-identified abstract regions: {paste(structure_analysis$abstract_regions, collapse=', ')}"))
                                              
                                              abstract_text <- raw_df %>%
                                                filter(page == 1) %>%
                                                arrange(y, x) %>%
                                                slice(structure_analysis$abstract_regions) %>%
                                                pull(text) %>%
                                                paste(collapse = " ")
                                              
                                            } else {
                                              # More aggressive manual search
                                              message("LLM didn't identify abstract regions, using aggressive search")
                                              
                                              # Look for ANY substantial text block that could be an abstract
                                              abstract_candidates <- raw_df %>%
                                                filter(page == 1) %>%
                                                arrange(y, x) %>%
                                                mutate(
                                                  could_be_abstract = case_when(
                                                    # Obvious abstract markers
                                                    str_detect(tolower(text), "^abstract|^summary") ~ 10,
                                                    
                                                    # Large text blocks with scientific content
                                                    nchar(text) > 200 & str_detect(text, "nanomaterials|nanocrystals|antibacterial") ~ 8,
                                                    
                                                    # Medium text blocks with multiple sentences
                                                    nchar(text) > 150 & str_count(text, "\\.") >= 2 ~ 6,
                                                    
                                                    # Any substantial paragraph
                                                    nchar(text) > 100 & str_count(text, "\\.") >= 1 ~ 4,
                                                    
                                                    TRUE ~ 0
                                                  )
                                                ) %>%
                                                filter(could_be_abstract >= 4) %>%
                                                arrange(desc(could_be_abstract), desc(nchar(text)))
                                              
                                              if (nrow(abstract_candidates) > 0) {
                                                abstract_text <- abstract_candidates %>% slice(1) %>% pull(text)
                                                message(glue("Found potential abstract: {str_trunc(abstract_text, 100)}..."))
                                              } else {
                                                # Final fallback: just take a large text block
                                                large_text <- raw_df %>%
                                                  filter(page == 1, nchar(text) > 50) %>%
                                                  arrange(desc(nchar(text))) %>%
                                                  slice(1) %>%
                                                  pull(text)
                                                
                                                if (length(large_text) > 0) {
                                                  abstract_text <- large_text
                                                  message("Using largest text block as abstract fallback")
                                                } else {
                                                  return("Abstract not found")
                                                }
                                              }
                                            }
                                            
                                            if (length(abstract_text) == 0 || nchar(abstract_text) == 0) {
                                              return("Abstract not found")
                                            }
                                            
                                            message(glue("Raw abstract length: {nchar(abstract_text)} characters"))
                                            
                                            # Clean the abstract
                                            clean_abstract <- abstract_text %>%
                                              str_replace_all("^(Abstract|ABSTRACT|Summary|SUMMARY):?\\s*", "") %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            message(glue("Cleaned abstract length: {nchar(clean_abstract)} characters"))
                                            return(clean_abstract)
                                          },
                                          
                                          extract_abstract_universal = function(raw_df, structure_analysis, model) { 
                                            message("=== Universal Abstract Extraction DEBUG ===")
                                            
                                            # CRITICAL: Always create text_clean early and handle missing columns safely
                                            page1_content <- raw_df %>%
                                              filter(page == 1) %>%
                                              mutate(
                                                x_center = if("x" %in% names(raw_df)) x + w/2 else 500,
                                                y_center = if("y" %in% names(raw_df)) y + h/2 else 500,
                                                area = if(all(c("w", "h") %in% names(raw_df))) w * h else 1000,
                                                text_clean = if("text" %in% names(raw_df)) str_replace_all(text, "\\s+", " ") %>% str_trim() else ""
                                              ) %>%
                                              filter(!is.na(text_clean), nchar(text_clean) > 0)
                                            
                                            # Early exit if no content
                                            if (nrow(page1_content) == 0) {
                                              message("No valid page 1 content found")
                                              return("Abstract not found")
                                            }
                                            
                                            # Safe format detection
                                            text_for_detection <- page1_content$text_clean %>% paste(collapse = " ") %>% tolower()
                                            
                                            is_nature_format <- str_detect(text_for_detection, "nature.*nanotechnology|nature.*communications")
                                            is_jac_format <- str_detect(text_for_detection, "journal.*antimicrobial.*chemotherapy")
                                            is_jsm_format <- str_detect(text_for_detection, "jsm nanotechnology|scimed.*central")
                                            
                                            message(glue("Format detection: Nature={is_nature_format}, JAC={is_jac_format}, JSM={is_jsm_format}"))
                                            
                                            # NATURE FORMAT
                                            if (is_nature_format) {
                                              message("Using Nature format abstract extraction")
                                              
                                              boxed_abstract_candidates <- page1_content %>%
                                                filter(
                                                  y >= 300 & y < 900,
                                                  nchar(text_clean) > 200,
                                                  area > 30000,
                                                  str_detect(text_clean, "Silver nanoparticles have already been successfully applied|antimicrobial technologies|biomedical.*products")
                                                ) %>%
                                                arrange(y)
                                              
                                              if (nrow(boxed_abstract_candidates) > 0) {
                                                message("Nature abstract candidates found:")
                                                # SAFE: Use mutate to create preview column
                                                boxed_abstract_candidates %>%
                                                  mutate(preview = str_trunc(text_clean, 100)) %>%
                                                  select(y, area, preview) %>%
                                                  print()
                                                
                                                abstract_text <- boxed_abstract_candidates %>% slice(1) %>% pull(text_clean)
                                                message(glue("Nature abstract found: {nchar(abstract_text)} chars"))
                                                return(abstract_text)
                                              }
                                            }
                                            
                                            # JSM FORMAT  
                                            if (is_jsm_format) {
                                              message("Using JSM format abstract extraction")
                                              
                                              jsm_abstract_candidates <- page1_content %>%
                                                filter(
                                                  str_detect(text_clean, "Escherichia coli can rapidly evolve resistance.*AgNP|experimental evolution.*demonstrate.*selection") |
                                                    (y >= 1000 & y < 1700 & nchar(text_clean) > 200 & 
                                                       !str_detect(text_clean, "Brentwood.*Biomedical|Department.*Nano|University.*USA"))
                                                ) %>%
                                                arrange(y)
                                              
                                              if (nrow(jsm_abstract_candidates) > 0) {
                                                message("JSM abstract candidates found:")
                                                # SAFE: Create preview properly
                                                jsm_abstract_candidates %>%
                                                  mutate(preview = str_trunc(text_clean, 100)) %>%
                                                  select(y, preview) %>%
                                                  print()
                                                
                                                # Take the one with abstract content
                                                abstract_text <- jsm_abstract_candidates %>%
                                                  filter(str_detect(text_clean, "Escherichia coli can rapidly evolve")) %>%
                                                  slice(1) %>%
                                                  pull(text_clean)
                                                
                                                if (length(abstract_text) == 0 || nchar(abstract_text) == 0) {
                                                  abstract_text <- jsm_abstract_candidates %>%
                                                    filter(!str_detect(text_clean, "Department|University|Institute")) %>%
                                                    arrange(desc(nchar(text_clean))) %>%
                                                    slice(1) %>%
                                                    pull(text_clean)
                                                }
                                                
                                                if (length(abstract_text) > 0 && nchar(abstract_text) > 0) {
                                                  message(glue("JSM abstract found: {nchar(abstract_text)} chars"))
                                                  return(abstract_text)
                                                }
                                              }
                                            }
                                            
                                            # JAC FORMAT (Structured Abstract)
                                            if (is_jac_format) {
                                              message("Using JAC structured abstract extraction")
                                              
                                              # Apply basic sorting for JAC
                                              page1_sorted <- page1_content %>% arrange(y, x)
                                              
                                              structured_components <- page1_sorted %>%
                                                filter(
                                                  str_detect(tolower(text_clean), "^objectives?:|^methods?:|^results?:|^conclusions?:")
                                                ) %>%
                                                arrange(y, x)
                                              
                                              if (nrow(structured_components) > 0) {
                                                message("JAC structured components found:")
                                                # SAFE: Create proper preview
                                                structured_components %>%
                                                  mutate(
                                                    preview = str_trunc(text_clean, 80),
                                                    component_type = case_when(
                                                      str_detect(tolower(text_clean), "^objectives?:") ~ "Objectives",
                                                      str_detect(tolower(text_clean), "^methods?:") ~ "Methods", 
                                                      str_detect(tolower(text_clean), "^results?:") ~ "Results",
                                                      str_detect(tolower(text_clean), "^conclusions?:") ~ "Conclusions",
                                                      TRUE ~ "Unknown"
                                                    )
                                                  ) %>%
                                                  select(component_type, preview) %>%
                                                  print()
                                                
                                                abstract_text <- structured_components %>%
                                                  pull(text_clean) %>%
                                                  paste(collapse = " ")
                                                
                                                message(glue("JAC structured abstract assembled: {nchar(abstract_text)} chars"))
                                                return(abstract_text)
                                              }
                                            }
                                            
                                            # GENERIC FORMAT
                                            message("Using generic abstract extraction")
                                            page1_sorted <- page1_content %>% arrange(y, x)
                                            
                                            # Look for explicit abstract markers
                                            explicit_abstracts <- page1_sorted %>%
                                              filter(str_detect(tolower(text_clean), "^abstract\\s*:?|^summary\\s*:?")) %>%
                                              arrange(y)
                                            
                                            if (nrow(explicit_abstracts) > 0) {
                                              abstract_text <- explicit_abstracts %>%
                                                slice(1) %>%
                                                pull(text_clean) %>%
                                                str_replace_all("^(Abstract|ABSTRACT|Summary|SUMMARY)\\s*:?\\s*", "")
                                              
                                              message("Found explicit abstract marker")
                                              return(abstract_text)
                                            }
                                            
                                            # Look for substantial paragraphs
                                            generic_candidates <- page1_sorted %>%
                                              filter(
                                                element_type %in% c("plain text") | is.na(element_type),
                                                nchar(text_clean) > 150,
                                                str_count(text_clean, "\\.") >= 2,
                                                !str_detect(tolower(text_clean), "^(highlights?|keywords?|introduction|university|department)")
                                              ) %>%
                                              arrange(desc(nchar(text_clean))) %>%
                                              slice(1:5)
                                            
                                            if (nrow(generic_candidates) > 0) {
                                              message("Generic abstract candidates found:")
                                              # SAFE: Create preview properly  
                                              generic_candidates %>%
                                                mutate(
                                                  preview = str_trunc(text_clean, 100),
                                                  text_length = nchar(text_clean)  # Create length column safely
                                                ) %>%
                                                select(text_length, preview) %>%
                                                print()
                                              
                                              abstract_text <- generic_candidates %>% slice(1) %>% pull(text_clean)
                                              message("Using generic abstract detection")
                                              return(abstract_text)
                                            }
                                            
                                            message("No suitable abstract found")
                                            return("Abstract not found")
                                          },
                                          
                                          extract_abstract_nature_format = function(raw_df) {
                                            message("=== Nature Format Abstract Detection ===")
                                            
                                            page1_data <- raw_df %>%
                                              filter(page == 1) %>%
                                              arrange(y, x) %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim(),
                                                area = w * h,
                                                y_position = y
                                              )
                                            
                                            # Look for the boxed abstract - usually a large text block early on the page
                                            boxed_abstract_candidates <- page1_data %>%
                                              filter(
                                                y < 900,  # Upper portion of page
                                                nchar(text_clean) > 200,  # Substantial text
                                                area > 30000,  # Large text block
                                                str_detect(text_clean, "silver nanoparticles.*applied.*biomedical|antimicrobial technologies|resistance.*antibacterial")
                                              ) %>%
                                              arrange(y)
                                            
                                            message("Boxed abstract candidates:")
                                            if (nrow(boxed_abstract_candidates) > 0) {
                                              boxed_abstract_candidates %>%
                                                mutate(preview = str_trunc(text_clean, 100)) %>%
                                                select(y, area, preview) %>%
                                                print()
                                              
                                              abstract_text <- boxed_abstract_candidates %>%
                                                slice(1) %>%
                                                pull(text_clean)
                                              
                                              return(abstract_text)
                                            }
                                            
                                            # Fallback: Look for any large text block that contains key abstract content
                                            fallback_candidates <- page1_data %>%
                                              filter(
                                                nchar(text_clean) > 300,
                                                str_detect(text_clean, "silver nanoparticles.*bacteria|resistance.*antibacterial|antimicrobial.*agent")
                                              ) %>%
                                              arrange(desc(nchar(text_clean)))
                                            
                                            if (nrow(fallback_candidates) > 0) {
                                              abstract_text <- fallback_candidates %>%
                                                slice(1) %>%
                                                pull(text_clean)
                                              
                                              message("Using fallback abstract detection")
                                              return(abstract_text)
                                            }
                                            
                                            return("Abstract not found")
                                          },
                                          
                                          # Enhanced extract_main_text_smart method with better reference detection
                                          extract_main_text_smart = function(raw_df, extracted_abstract = NULL) {
                                            message("=== ENHANCED Smart Main Text Extraction ===")
                                            
                                            # Detect document format first
                                            doc_format <- self$detect_document_format(raw_df)
                                            is_mdpi <- doc_format == "mdpi" || any(str_detect(tolower(raw_df$text), "mdpi|biomolecules|materials|nanomaterials"))
                                            
                                            if (is_mdpi) {
                                              message("MDPI format detected - using simplified approach")
                                              return(self$extract_main_text_mdpi_simplified(raw_df))
                                            }
                                            
                                            # Apply proper reading order to ALL pages with NA filtering
                                            all_text <- raw_df %>%
                                              filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                              filter(!is.na(text), nchar(str_trim(text)) > 0) %>%
                                              group_by(page) %>%
                                              group_modify(~ {
                                                page_df <- .x
                                                page_num <- .y$page
                                                
                                                page_df <- page_df %>%
                                                  mutate(
                                                    x_center = x + w/2,
                                                    y_center = y + h/2,
                                                    area = w * h
                                                  )
                                                
                                                layout_info <- detect_page_layout_universal(page_df)
                                                
                                                if (layout_info$columns == 2) {
                                                  page_df <- page_df %>%
                                                    mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
                                                    arrange(column, y, x) %>%
                                                    select(-x_center, -y_center, -area, -column)
                                                } else {
                                                  page_df <- page_df %>%
                                                    arrange(y, x) %>%
                                                    select(-x_center, -y_center, -area)
                                                }
                                                
                                                return(page_df)
                                              }) %>%
                                              ungroup() %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim(),
                                                text_lower = tolower(text_clean),
                                                row_id = row_number()
                                              ) %>%
                                              filter(!is.na(text_clean), nchar(text_clean) > 0)
                                            
                                            message(glue("Sorted text: {nrow(all_text)} regions"))
                                            
                                            # Remove basic MDPI front matter first
                                            if (is_mdpi) {
                                              message("Removing MDPI front matter...")
                                              
                                              all_text <- all_text %>%
                                                filter(
                                                  # Remove article metadata headers
                                                  !str_detect(text_clean, "^Article\\s+Article\\s+check for updates"),
                                                  !str_detect(text_clean, "^Academic Editor:"),
                                                  !str_detect(text_clean, "^(Received|Revised|Accepted|Published):"),
                                                  !str_detect(text_clean, "^Citation:"),
                                                  !str_detect(text_clean, "^Copyright:"),
                                                  !str_detect(text_clean, "Licensee MDPI"),
                                                  !str_detect(text_clean, "This article is an open access article"),
                                                  !str_detect(text_clean, "Creative Commons Attribution"),
                                                  
                                                  # Remove duplicated title/author blocks
                                                  !(page == 1 & row_id > 10 & str_detect(text_clean, "^Article\\s+[A-Z]")),
                                                  !(page == 1 & row_id > 10 & str_detect(text_clean, "^[A-Z][a-z]+\\s+[A-Z]\\.\\s+[A-Z][a-z]+")),
                                                  
                                                  # Remove affiliation blocks with emails
                                                  !str_detect(text_clean, "@[a-zA-Z0-9.-]+\\.[a-z]{2,}"),
                                                  !str_detect(text_clean, "^\\d+\\s+[A-Z][a-z]+(\\s+[A-Z][a-z]+)*"),
                                                  !str_detect(text_clean, "^\\d+\\s+(Clinical|Botany|Microbiology|Sciences|Department|Faculty)"),
                                                  
                                                  # Remove correspondence lines
                                                  !str_detect(text_clean, "^\\*?\\s*Correspondence:")
                                                ) %>%
                                                mutate(row_id = row_number())  # Re-number
                                              
                                              message(glue("After front matter removal: {nrow(all_text)} regions"))
                                            }
                                            
                                            # Find introduction start
                                            start_idx <- NULL
                                            
                                            intro_candidates <- all_text %>%
                                              filter(!is.na(text_clean), !is.na(text_lower)) %>%
                                              mutate(
                                                has_mdpi_intro = is_mdpi & str_detect(text_clean, "^Abstract:\\s+\\w+"),
                                                has_general_intro = str_detect(text_lower, "introduction|^1\\.\\s+introduction"),
                                                has_numbered_start = str_detect(text_clean, "^1\\.\\s+[A-Z]"),
                                                is_substantial = nchar(text_clean) > 100,
                                                starts_properly = str_detect(text_clean, "^[A-Z][a-z]"),
                                                
                                                intro_score = as.numeric(has_mdpi_intro) * 10 +
                                                  as.numeric(has_general_intro) * 5 +
                                                  as.numeric(has_numbered_start) * 3 +
                                                  as.numeric(is_substantial) * 1 +
                                                  as.numeric(starts_properly) * 1
                                              ) %>%
                                              filter(intro_score >= 3) %>%
                                              arrange(desc(intro_score), row_id)
                                            
                                            if (nrow(intro_candidates) > 0) {
                                              start_idx <- intro_candidates$row_id[1]
                                              start_text <- intro_candidates$text_clean[1]
                                              message(glue("Found introduction start at row {start_idx}: {str_trunc(start_text, 100)}"))
                                            }
                                            
                                            if (is.null(start_idx)) {
                                              start_idx <- min(15, nrow(all_text))
                                              message(glue("Using fallback start at row {start_idx}"))
                                            }
                                            
                                            # NEW APPROACH: Find the END of conclusions and cut everything after
                                            end_idx <- nrow(all_text)  
                                            
                                            if (is_mdpi) {
                                              message("Looking for end of conclusions...")
                                              
                                              # Find text that contains "clinical trial"
                                              clinical_trial_matches <- all_text %>%
                                                filter(str_detect(text_clean, "clinical trial\\.?")) %>%
                                                arrange(row_id)
                                              
                                              if (nrow(clinical_trial_matches) > 0) {
                                                for (match_idx in 1:nrow(clinical_trial_matches)) {
                                                  clinical_trial_row <- clinical_trial_matches$row_id[match_idx]
                                                  full_text <- clinical_trial_matches$text_clean[match_idx]
                                                  
                                                  message(glue("Found 'clinical trial' at row {clinical_trial_row}"))
                                                  
                                                  # Check if unwanted sections are in the SAME text region
                                                  if (str_detect(full_text, "Supplementary Materials:|Author Contributions:|Funding:|References")) {
                                                    
                                                    # Find where "clinical trial" ends in this text
                                                    clinical_trial_pos <- str_locate(full_text, "clinical trial\\.?")[2]
                                                    
                                                    if (!is.na(clinical_trial_pos)) {
                                                      # Extract only up to the end of "clinical trial"
                                                      clean_text <- str_sub(full_text, 1, clinical_trial_pos)
                                                      
                                                      # Replace the text in all_text with the truncated version
                                                      all_text$text_clean[clinical_trial_row] <- clean_text
                                                      
                                                      # Set end to this row
                                                      end_idx <- clinical_trial_row
                                                      message(glue("Truncated text at row {clinical_trial_row} after 'clinical trial'"))
                                                      break
                                                    }
                                                  } else {
                                                    # If no unwanted content in same region, look for it in subsequent regions
                                                    for (i in (clinical_trial_row + 1):min(nrow(all_text), clinical_trial_row + 5)) {
                                                      if (i <= nrow(all_text)) {
                                                        text_check <- all_text$text_clean[i]
                                                        
                                                        if (!is.na(text_check) && 
                                                            str_detect(text_check, "Supplementary Materials:|Author Contributions:|Funding:|^References\\s*$|\\[CrossRef\\]")) {
                                                          end_idx <- i - 1
                                                          message(glue("Found unwanted content at row {i}, ending at row {end_idx}"))
                                                          break
                                                        }
                                                      }
                                                    }
                                                    break
                                                  }
                                                }
                                              }
                                            }
                                            
                                            # Extract content
                                            start_idx <- max(1, start_idx)
                                            end_idx <- min(end_idx, nrow(all_text))
                                            
                                            if (start_idx > end_idx) {
                                              message("Warning: start > end, using full content")
                                              start_idx <- 1
                                              end_idx <- nrow(all_text)
                                            }
                                            
                                            main_content <- all_text %>%
                                              slice(start_idx:end_idx) %>%
                                              pull(text_clean) %>%
                                              paste(collapse = " ")
                                            
                                            # Final cleaning for MDPI
                                            if (is_mdpi) {
                                              main_content <- main_content %>%
                                                # Remove any remaining figure/table references
                                                str_replace_all("\\(Figure \\d+[a-z]?\\)", "") %>%
                                                str_replace_all("\\(Table \\d+\\)", "") %>%
                                                # Remove inline citations
                                                str_replace_all("\\[\\d+–\\d+\\]", "") %>%
                                                str_replace_all("\\[\\d+,\\s*\\d+\\]", "") %>%
                                                str_replace_all("\\[\\d+\\]", "") %>%
                                                # Clean up spacing
                                                str_replace_all("\\s+", " ") %>%
                                                str_trim()
                                            }
                                            
                                            # Remove duplicated sentences
                                            main_content <- main_content %>%
                                              str_split("\\. ") %>%
                                              map(~ unique(.x)) %>%
                                              map_chr(~ paste(.x, collapse = ". ")) %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            # Create original content
                                            original_content <- all_text %>%
                                              pull(text_clean) %>%
                                              paste(collapse = " ")
                                            
                                            message(glue("EXTRACTION SUMMARY:"))
                                            message(glue("  Format: {doc_format}"))
                                            message(glue("  Start: row {start_idx}"))
                                            message(glue("  End: row {end_idx}"))
                                            message(glue("  Extracted: {end_idx - start_idx + 1} regions"))
                                            message(glue("  Main content: {nchar(main_content)} chars"))
                                            
                                            return(list(
                                              main = main_content,
                                              original = original_content,
                                              boundaries = list(start = start_idx, end = end_idx)
                                            ))
                                          },
                                          
                                          extract_body_text_only = function(raw_df) {
                                            message("=== DEBUG: Body text extraction (no front matter) ===")
                                            
                                            all_text <- raw_df %>%
                                              filter(nchar(str_trim(text)) > 10) %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " "),
                                                row_id = row_number()
                                              ) %>%
                                              arrange(page, y, x)
                                            
                                            # Find introduction
                                            intro_candidates <- all_text %>%
                                              mutate(
                                                has_intro_word = str_detect(tolower(text_clean), "introduction|background|infectious diseases|noble metal"),
                                                starts_with_number = str_detect(text_clean, "^\\d+\\.?\\s"),
                                                is_substantial = nchar(text_clean) > 100,
                                                is_early_page = page <= 2,
                                                intro_score = as.numeric(has_intro_word) * 2 + as.numeric(starts_with_number) * 1 + 
                                                  as.numeric(is_substantial) * 1 + as.numeric(is_early_page) * 1
                                              ) %>%
                                              filter(intro_score >= 2) %>%
                                              arrange(desc(intro_score), page, row_id)
                                            
                                            start_row <- if (nrow(intro_candidates) > 0) {
                                              intro_candidates %>% slice(1) %>% pull(row_id)
                                            } else {
                                              10  # Skip first 10 regions
                                            }
                                            
                                            # CONSERVATIVE end matter detection - only real section headers
                                            end_candidates <- all_text %>%
                                              mutate(
                                                is_references = str_detect(str_trim(text_clean), "^\\s*References\\s*$|^\\s*REFERENCES\\s*$"),
                                                is_acknowledgments = str_detect(str_trim(text_clean), "^\\s*Acknowledgments?\\s*$"),
                                                is_short = nchar(str_trim(text_clean)) < 50,
                                                is_late = page >= 6,
                                                is_not_doi = !str_detect(tolower(text_clean), "doi|www\\.|http"),
                                                end_score = (as.numeric(is_references) + as.numeric(is_acknowledgments)) * 
                                                  as.numeric(is_short) * as.numeric(is_late) * as.numeric(is_not_doi)
                                              ) %>%
                                              filter(end_score >= 1) %>%
                                              arrange(row_id)
                                            
                                            if (nrow(end_candidates) > 0) {
                                              end_row <- end_candidates %>% slice(1) %>% pull(row_id)
                                              body_content <- all_text %>%
                                                slice(start_row:(end_row - 1)) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                            } else {
                                              # No clear end - filter out page headers/footers
                                              filtered_content <- all_text %>%
                                                slice(start_row:nrow(all_text)) %>%
                                                filter(
                                                  !str_detect(text_clean, "^\\s*NATURE COMMUNICATIONS.*\\|.*DOI"),
                                                  !str_detect(text_clean, "^\\s*URE COMMUNICATIONS.*\\|.*DOI"),
                                                  nchar(text_clean) > 20
                                                ) %>%
                                                pull(text_clean) %>%
                                                paste(collapse = " ") %>%
                                                str_trim()
                                              
                                              body_content <- filtered_content
                                            }
                                            
                                            if (nchar(body_content) < 1000) {
                                              message("WARNING: Body text too short, using full main text")
                                              body_content <- self$extract_main_text_smart(raw_df)
                                            }
                                            
                                            message(glue("Body text length: {nchar(body_content)} characters"))
                                            return(body_content)
                                          },
                                          
                                          extract_title_universal = function(raw_df, structure_analysis, model) {
                                            message("=== Universal Title Extraction ===")
                                            
                                            # EXCLUDE abandon elements from title search - they contain headers/metadata
                                            title_candidates <- raw_df %>%
                                              filter(page == 1) %>%
                                              # CRITICAL: Only use plain text and title elements for title detection
                                              filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                              # Exclude obvious journal headers and metadata
                                              filter(!str_detect(tolower(text), "^(nature|communications|journal|doi|article|open|paper)\\s*$")) %>%
                                              filter(!str_detect(text, "^(DOI:|www\\.|http)")) %>%
                                              mutate(
                                                # Enhanced title scoring
                                                title_score = case_when(
                                                  element_type == "title" & nchar(text) > 30 ~ 10,  # High priority for YOLO-detected titles
                                                  y < 400 & nchar(text) > 40 & nchar(text) < 300 ~ 8,  # Good position and length
                                                  y < 600 & nchar(text) > 30 & str_detect(text, "[A-Z].*[a-z]") ~ 6,  # Title-like
                                                  TRUE ~ 0
                                                ),
                                                
                                                # Penalty for metadata-like content
                                                title_score = title_score - case_when(
                                                  str_detect(tolower(text), "journal|doi|volume|issue|page|copyright") ~ 5,
                                                  str_detect(text, "\\d{4}|@|\\.edu|\\.com") ~ 3,
                                                  nchar(text) < 20 ~ 2,
                                                  TRUE ~ 0
                                                )
                                              ) %>%
                                              filter(title_score > 3) %>%
                                              arrange(desc(title_score), y)
                                            
                                            if (nrow(title_candidates) > 0) {
                                              title_text <- title_candidates %>% slice(1) %>% pull(text)
                                              message(glue("Selected title: {str_trunc(title_text, 60)}"))
                                            } else {
                                              message("No suitable title candidates found")
                                              return("Title not found")
                                            }
                                            
                                            # Clean the title
                                            clean_title <- title_text %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            return(clean_title)
                                          },
                                          
                                          extract_content_simple_universal = function(raw_df) {
                                            message("=== SIMPLIFIED UNIVERSAL EXTRACTION ===")
                                            
                                            # Sort all text by basic reading order (top to bottom, left to right per page)
                                            all_content <- raw_df %>%
                                              filter(!is.na(text), nchar(str_trim(text)) > 5) %>%
                                              arrange(page, y, x) %>%  # Simple sort - works better than complex column detection
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim(),
                                                row_id = row_number()
                                              )
                                            
                                            message(glue("Processed {nrow(all_content)} text regions"))
                                            
                                            # TITLE: Look for title-like text on page 1
                                            title_candidates <- all_content %>%
                                              filter(
                                                page == 1,
                                                nchar(text_clean) > 20 & nchar(text_clean) < 300,
                                                str_detect(text_clean, "Selection.*Silver|Bacterial.*resistance.*silver|Silver.*Nanoparticle.*Resistance"),
                                                !str_detect(tolower(text_clean), "journal|doi|university|department")
                                              ) %>%
                                              arrange(y) %>%
                                              slice(1)
                                            
                                            # AUTHORS: Look for text with multiple proper names
                                            author_candidates <- all_content %>%
                                              filter(
                                                page == 1,
                                                str_count(text_clean, "[A-Z][a-z]+\\s+[A-Z][a-z]+") >= 3,  # Multiple names
                                                nchar(text_clean) > 50,
                                                !str_detect(text_clean, "Department|University|Abstract|Escherichia|resistance")
                                              ) %>%
                                              arrange(y) %>%
                                              slice(1)
                                            
                                            # ABSTRACT: Look for substantial scientific text blocks
                                            abstract_candidates <- all_content %>%
                                              filter(
                                                page == 1,
                                                nchar(text_clean) > 150,
                                                str_detect(text_clean, "Escherichia coli.*resistance|experimental evolution.*demonstrate|AgNP.*selection"),
                                                !str_detect(text_clean, "Department|University|Institute|^Silver,")  # Exclude affiliations and intro
                                              ) %>%
                                              arrange(desc(nchar(text_clean))) %>%
                                              slice(1)
                                            
                                            # INTRODUCTION START: Look for clear intro patterns
                                            intro_patterns <- c(
                                              "Silver, either in ionic form.*Ag.*nanoparticles",
                                              "^Introduction\\s*$",
                                              "^Silver.*ionic.*form.*AgNPs.*toxic.*bacteria"
                                            )
                                            
                                            intro_start_idx <- NULL
                                            for (i in 1:nrow(all_content)) {
                                              text_check <- all_content$text_clean[i]
                                              for (pattern in intro_patterns) {
                                                if (str_detect(text_check, pattern)) {
                                                  intro_start_idx <- i
                                                  message(glue("Found intro at row {i}: {str_trunc(text_check, 80)}"))
                                                  break
                                                }
                                              }
                                              if (!is.null(intro_start_idx)) break
                                            }
                                            
                                            # Fallback intro detection
                                            if (is.null(intro_start_idx)) {
                                              intro_fallback <- all_content %>%
                                                filter(
                                                  nchar(text_clean) > 100,
                                                  str_detect(text_clean, "^Silver.*toxic.*bacteria|Silver.*nanoparticles.*bacteria"),
                                                  !str_detect(text_clean, "Department|University")
                                                ) %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              intro_start_idx <- if(length(intro_fallback) > 0) intro_fallback else 25
                                            }
                                            
                                            # Extract main content
                                            main_content <- all_content %>%
                                              slice(intro_start_idx:n()) %>%
                                              pull(text_clean) %>%
                                              paste(collapse = " ") %>%
                                              # Remove common duplication patterns
                                              str_replace_all("(\\. [^.]{50,}?)\\1+", "\\1") %>%  # Remove sentence duplicates
                                              str_replace_all("ABBREVIATIONS.*?(?=(Introduction|Silver|[A-Z][a-z]+\\s+[a-z]+))", "") %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            # Return results
                                            results <- list(
                                              title = if(nrow(title_candidates) > 0) title_candidates$text_clean else "Title not found",
                                              authors = if(nrow(author_candidates) > 0) author_candidates$text_clean else "Authors not found", 
                                              abstract = if(nrow(abstract_candidates) > 0) abstract_candidates$text_clean else "Abstract not found",
                                              main_content = main_content
                                            )
                                            
                                            # Debug output
                                            message("EXTRACTION RESULTS:")
                                            message(glue("  Title: {str_trunc(results$title, 60)}"))
                                            message(glue("  Authors: {str_trunc(results$authors, 60)}"))
                                            message(glue("  Abstract: {str_trunc(results$abstract, 60)}"))
                                            message(glue("  Main content: {nchar(results$main_content)} chars"))
                                            
                                            return(results)
                                          },
                                          
                                          extract_main_text_mdpi_simplified = function(raw_df) {
                                            message("=== MDPI SIMPLIFIED EXTRACTION (X-coordinate aware) ===")
                                            
                                            # Simple page-by-page, top-to-bottom reading order
                                            all_text <- raw_df %>%
                                              filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                              filter(!is.na(text), nchar(str_trim(text)) > 0) %>%
                                              arrange(page, y, x) %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim(),
                                                text_lower = tolower(text_clean),
                                                row_id = row_number(),
                                                x_center = x + w/2  # Calculate center X position
                                              ) %>%
                                              filter(!is.na(text_clean), nchar(text_clean) > 0)
                                            
                                            message(glue("MDPI: Sorted {nrow(all_text)} regions"))
                                            
                                            # Determine the X-coordinate zones by looking at page 1
                                            page1_data <- all_text %>% filter(page == 1)
                                            
                                            # Find the main content X zone (right-shifted area)
                                            # Look for the abstract or title area to determine main content X position
                                            main_content_candidates <- page1_data %>%
                                              filter(str_detect(text_lower, "abstract|staphylococcus|methicillin")) %>%
                                              filter(nchar(text_clean) > 100)  # Substantial content
                                            
                                            if (nrow(main_content_candidates) > 0) {
                                              main_content_x_min <- min(main_content_candidates$x, na.rm = TRUE)
                                              main_content_x_center <- mean(main_content_candidates$x_center, na.rm = TRUE)
                                            } else {
                                              # Fallback: assume main content starts around X=400 based on typical MDPI layout
                                              main_content_x_min <- 300
                                              main_content_x_center <- 500
                                            }
                                            
                                            message(glue("MDPI main content zone: X >= {main_content_x_min}, center ~{round(main_content_x_center)}"))
                                            
                                            # Filter to only include content from the main content zone (right-shifted area)
                                            # This automatically excludes the left minicolumn and affiliations
                                            main_zone_text <- all_text %>%
                                              filter(x >= main_content_x_min - 50) %>%  # Small buffer for text alignment variations
                                              mutate(row_id = row_number())
                                            
                                            message(glue("After X-zone filtering: {nrow(main_zone_text)} regions"))
                                            
                                            # Find start - look for "Abstract:"
                                            start_idx <- NULL
                                            
                                            for (i in 1:min(20, nrow(main_zone_text))) {
                                              text_check <- main_zone_text$text_clean[i]
                                              text_lower <- main_zone_text$text_lower[i]
                                              
                                              if (!is.na(text_check) && !is.na(text_lower)) {
                                                if (str_detect(text_lower, "^abstract:?\\s") || 
                                                    str_detect(text_check, "^Abstract:")) {
                                                  start_idx <- i
                                                  message(glue("Found MDPI Abstract start at row {i}: {str_trunc(text_check, 80)}"))
                                                  break
                                                }
                                              }
                                            }
                                            
                                            if (is.null(start_idx)) {
                                              # Fallback: look for substantial scientific content
                                              for (i in 1:min(20, nrow(main_zone_text))) {
                                                text_check <- main_zone_text$text_clean[i]
                                                if (nchar(text_check) > 100 && 
                                                    str_detect(text_check, "Methicillin-resistant|MRSA|silver nanoparticles")) {
                                                  start_idx <- i
                                                  message(glue("Found MDPI content start (fallback) at row {i}"))
                                                  break
                                                }
                                              }
                                            }
                                            
                                            if (is.null(start_idx)) {
                                              start_idx <- 3  # Final fallback
                                              message("Using final fallback start for MDPI")
                                            }
                                            
                                            # Find end - look for content that shifts back to left X-coordinate zone
                                            # This catches "References" which moves back to the left
                                            end_idx <- nrow(main_zone_text)  # Default
                                            
                                            # Look for the shift back to left zone (References section)
                                            for (i in max(start_idx + 20, round(nrow(main_zone_text) * 0.7)):nrow(main_zone_text)) {
                                              text_check <- main_zone_text$text_clean[i]
                                              text_lower <- main_zone_text$text_lower[i]
                                              x_pos <- main_zone_text$x[i]
                                              
                                              if (!is.na(text_check) && !is.na(text_lower)) {
                                                # Check if this is "References" AND it has shifted back left
                                                if (str_detect(text_lower, "^references?\\s*$") && 
                                                    x_pos < (main_content_x_min + 50)) {  # Shifted back to left zone
                                                  end_idx <- i - 1
                                                  message(glue("Found MDPI References (X-shift back) at row {i}, ending at {end_idx}"))
                                                  break
                                                }
                                                
                                                # Also stop at explicit end-of-content sections that are still in right zone
                                                if (str_detect(text_lower, "^supplementary materials?:|^author contributions?:|^funding:|^institutional review")) {
                                                  end_idx <- i - 1
                                                  message(glue("Found MDPI end section at row {i}, ending at {end_idx}"))
                                                  break
                                                }
                                              }
                                            }
                                            
                                            # Extract content
                                            start_idx <- max(1, start_idx)
                                            end_idx <- min(end_idx, nrow(main_zone_text))
                                            
                                            if (start_idx > end_idx) {
                                              message("Warning: Invalid MDPI boundaries")
                                              start_idx <- 1
                                              end_idx <- round(nrow(main_zone_text) * 0.8)
                                            }
                                            
                                            main_content <- main_zone_text %>%
                                              slice(start_idx:end_idx) %>%
                                              pull(text_clean) %>%
                                              paste(collapse = " ")
                                            
                                            # MDPI-specific cleaning
                                            main_content <- main_content %>%
                                              str_replace_all("\\(Figure \\d+[a-z]?\\)", "") %>%
                                              str_replace_all("\\(Table \\d+\\)", "") %>%
                                              str_replace_all("\\[\\d+–\\d+\\]", "") %>%
                                              str_replace_all("\\[\\d+,\\s*\\d+\\]", "") %>%
                                              str_replace_all("\\[\\d+\\]", "") %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            # Remove duplicated sentences
                                            main_content <- main_content %>%
                                              str_split("\\. ") %>%
                                              map(~ unique(.x)) %>%
                                              map_chr(~ paste(.x, collapse = ". ")) %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            original_content <- all_text %>%
                                              pull(text_clean) %>%
                                              paste(collapse = " ")
                                            
                                            message(glue("MDPI EXTRACTION SUMMARY:"))
                                            message(glue("  Main content X-zone: >= {main_content_x_min}"))
                                            message(glue("  Start: row {start_idx}"))  
                                            message(glue("  End: row {end_idx}"))
                                            message(glue("  Extracted: {end_idx - start_idx + 1} regions"))
                                            message(glue("  Main content: {nchar(main_content)} chars"))
                                            
                                            return(list(
                                              main = main_content,
                                              original = original_content,
                                              boundaries = list(start = start_idx, end = end_idx)
                                            ))
                                          },
                                          
                                          # Helper methods
                                          detect_section_header = function(text) {
                                            patterns <- c(
                                              "^\\d+\\. [A-Z]",
                                              "^\\d+ [A-Z]",
                                              "^[A-Z]{2,}$",
                                              "^[A-Z][a-z]+ [A-Z][a-z]+",
                                              "^(Introduction|Methods|Results|Discussion|Conclusion)$",
                                              "^(Materials and Methods|Results and Discussion)$"
                                            )
                                            any(sapply(patterns, function(pattern) str_detect(text, pattern)))
                                          },
                                          
                                          detect_body_start = function(text) {
                                            start_patterns <- c(
                                              "^(INTRODUCTION|Introduction)$",
                                              "^1\\. ",
                                              "^1 ",
                                              "^(Background|Objectives?|Aims?)$",
                                              "^(Materials and Methods|Methods)$"
                                            )
                                            any(sapply(start_patterns, function(pattern) str_detect(text, pattern)))
                                          },
                                          
                                          detect_body_end = function(text) {
                                            end_patterns <- c(
                                              "^(References|REFERENCES|Bibliography|BIBLIOGRAPHY)$",
                                              "^(Acknowledgments?|ACKNOWLEDGMENTS?)$",
                                              "^(Conflicts? of Interest|CONFLICTS? OF INTEREST)$",
                                              "^(Competing Interests|COMPETING INTERESTS)$",
                                              "^(Appendix|APPENDIX|Supplementary|SUPPLEMENTARY)$",
                                              "^(Supporting Information|SUPPORTING INFORMATION)$"
                                            )
                                            any(sapply(end_patterns, function(pattern) str_detect(text, pattern)))
                                          },
                                          
                                          detect_front_matter = function(text, y_pos, page_num) {
                                            # Fix vectorized condition by using vectorized functions
                                            result <- rep(FALSE, length(text))
                                            
                                            # Only check page 1 for front matter
                                            page_1_mask <- page_num == 1
                                            
                                            if (!any(page_1_mask)) return(result)
                                            
                                            # Apply logic only to page 1 elements
                                            text_lower <- tolower(text[page_1_mask])
                                            y_page_1 <- y_pos[page_1_mask]
                                            text_page_1 <- text[page_1_mask]
                                            
                                            # Vectorized detection for page 1 elements
                                            is_title <- y_page_1 < 600 & 
                                              nchar(text_page_1) > 20 & 
                                              nchar(text_page_1) < 300 &
                                              !str_detect(text_lower, "^(introduction|methods|results)")
                                            
                                            is_authors <- str_detect(text_page_1, "([A-Z][a-z]+\\s+){1,}[A-Z][a-z]+") &
                                              (str_detect(text_page_1, "\\d+|@|university|department") | y_page_1 < 800)
                                            
                                            is_abstract <- str_detect(text_lower, "abstract|summary") |
                                              (nchar(text_page_1) > 200 & 
                                                 y_page_1 > 400 & 
                                                 y_page_1 < 1200 & 
                                                 str_count(text_page_1, "\\.") >= 3)
                                            
                                            is_affiliations <- str_detect(text_lower, "university|department|institute|school") &
                                              nchar(text_page_1) > 50
                                            
                                            # Set results for page 1 elements
                                            result[page_1_mask] <- is_title | is_authors | is_abstract | is_affiliations
                                            
                                            return(result)
                                          },
                                          
                                          detect_end_matter = function(text, page_num, text_length) {
                                            # Fix vectorized condition
                                            result <- rep(FALSE, length(text))
                                            
                                            # Only check pages 3+ for end matter
                                            late_pages_mask <- page_num >= 3
                                            
                                            if (!any(late_pages_mask)) return(result)
                                            
                                            # Apply logic only to late page elements
                                            text_lower <- tolower(str_trim(text[late_pages_mask]))
                                            text_len <- text_length[late_pages_mask]
                                            text_late <- text[late_pages_mask]
                                            
                                            # Vectorized detection
                                            is_references <- str_detect(text_lower, "^references?$|^bibliography$") & text_len < 50
                                            
                                            is_acknowledgments <- str_detect(text_lower, "^acknowledgments?$|^acknowledgements?$") & text_len < 50
                                            
                                            is_reference_item <- str_detect(text_late, "^\\d+\\.\\s") & 
                                              text_len < 300 &
                                              str_detect(text_late, "et al\\.|journal|proc\\.|vol\\.")
                                            
                                            is_author_info <- str_detect(text_lower, "author contribution|competing interest|data availability") &
                                              text_len < 200
                                            
                                            # Set results for late page elements
                                            result[late_pages_mask] <- is_references | is_acknowledgments | is_reference_item | is_author_info
                                            
                                            return(result)
                                          },
                                          
                                          detect_front_matter_simple = function(text, y_pos, page_num) {
                                            if (page_num > 1) return(FALSE)
                                            
                                            text_lower <- tolower(text)
                                            
                                            # Title detection
                                            is_title <- y_pos < 600 && nchar(text) > 20 && nchar(text) < 300 &&
                                              !str_detect(text_lower, "^(introduction|methods|results)")
                                            
                                            # Author detection
                                            is_authors <- str_detect(text, "([A-Z][a-z]+\\s+){1,}[A-Z][a-z]+") &&
                                              (str_detect(text, "\\d+|@|university|department") || y_pos < 800)
                                            
                                            # Abstract detection
                                            is_abstract <- str_detect(text_lower, "abstract|summary") ||
                                              (nchar(text) > 200 && y_pos > 400 && y_pos < 1200 && str_count(text, "\\.") >= 3)
                                            
                                            # Affiliations
                                            is_affiliations <- str_detect(text_lower, "university|department|institute|school") &&
                                              nchar(text) > 50
                                            
                                            return(is_title || is_authors || is_abstract || is_affiliations)
                                          },
                                          
                                          detect_end_matter_simple = function(text, page_num, text_length) {
                                            if (page_num < 3) return(FALSE)
                                            
                                            text_lower <- tolower(str_trim(text))
                                            
                                            # References
                                            is_references <- str_detect(text_lower, "^references?$|^bibliography$") && text_length < 50
                                            
                                            # Acknowledgments
                                            is_acknowledgments <- str_detect(text_lower, "^acknowledgments?$") && text_length < 50
                                            
                                            # Reference items
                                            is_reference_item <- str_detect(text, "^\\d+\\.\\s") && text_length < 300 &&
                                              str_detect(text, "et al\\.|journal|proc\\.|vol\\.")
                                            
                                            # Author info
                                            is_author_info <- str_detect(text_lower, "author contribution|competing interest|data availability") &&
                                              text_length < 200
                                            
                                            return(is_references || is_reference_item || is_acknowledgments || is_author_info)
                                          },
                                          
                                          classify_content_type = function(text, text_length) {
                                            case_when(
                                              str_detect(text, "bioRxiv|doi:|preprint|copyright|license") ~ "metadata",
                                              str_detect(text, "^(TITLE|AUTHORS?|ABSTRACT|SUMMARY)$") ~ "front_matter",
                                              str_detect(text, "Department of|University of|@.*\\.edu") ~ "affiliations",
                                              self$detect_section_header(text) ~ "section_header",
                                              self$detect_body_end(text) ~ "end_matter",
                                              text_length > 200 & str_detect(text, "\\. ") ~ "body_text",
                                              str_detect(text, "^\\d+\\. ") & text_length < 100 ~ "reference_item",
                                              TRUE ~ "unknown"
                                            )
                                          },
                                          
                                          classify_content_for_extraction = function(text, page_num, y_pos, text_length) {
                                            text_lower <- tolower(text)
                                            
                                            # Front matter detection (page 1)
                                            if (page_num == 1) {
                                              if (str_detect(text, "DOI:|doi:|www\\.|http")) return("metadata")
                                              if (y_pos < 400 && text_length < 300) return("title")
                                              if (str_detect(text, "([A-Z][a-z]+\\s+){2,}") && str_detect(text, "@|university")) return("authors")
                                              if (str_detect(text_lower, "abstract|summary") || (text_length > 200 && y_pos > 400 && y_pos < 1000)) return("abstract")
                                              if (str_detect(text_lower, "university|department|institute") && text_length > 50) return("affiliations")
                                            }
                                            
                                            # End matter detection (later pages)
                                            if (page_num >= 3) {
                                              # References
                                              if (str_detect(text, "^\\d+\\.\\s.*et al\\.|journal|proc\\.|vol\\.") && text_length < 500) return("reference")
                                              if (str_detect(text_lower, "^references?$|^bibliography$") && text_length < 100) return("reference_header")
                                              
                                              # Methods sections (often not needed for analysis)
                                              if (str_detect(text_lower, "chemical reagents|preparation and characterization|statistical analysis")) return("methods_detail")
                                              if (str_detect(text_lower, "^methods$|materials and methods") && text_length < 100) return("methods_header")
                                              
                                              # Author contributions, licensing, etc.
                                              if (str_detect(text_lower, "author contribution|competing interest|data availability|supplementary")) return("publication_info")
                                              if (str_detect(text, "Creative Commons|license|copyright|©") && text_length > 100) return("copyright")
                                              
                                              # Acknowledgments
                                              if (str_detect(text_lower, "acknowledgment|funding|supported by|grant")) return("acknowledgments")
                                            }
                                            
                                            # Main content detection
                                            if (str_detect(text_lower, "^introduction$|^1\\.|infectious diseases|background")) return("introduction")
                                            if (str_detect(text_lower, "^results$|^discussion$|^conclusion")) return("section_header")
                                            if (text_length > 100 && str_count(text, "\\.") >= 2) return("body_text")
                                            
                                            return("unknown")
                                          },
                                          
                                          classify_content_universal = function(text, text_length, y_pos, element_type = "unknown") {
                                            text_lower <- tolower(text)
                                            
                                            # Multi-criteria scoring system
                                            scores <- list(
                                              title = 0,
                                              authors = 0,
                                              abstract = 0,
                                              section_header = 0,
                                              body_text = 0,
                                              metadata = 0,
                                              reference = 0
                                            )
                                            
                                            # Title scoring
                                            if (y_pos < 500 & text_length > 20 & text_length < 300) scores$title <- scores$title + 2
                                            if (str_detect(text, "^[A-Z].*[a-z]") & !str_detect(text_lower, "^(journal|article|doi)")) scores$title <- scores$title + 1
                                            if (element_type == "title") scores$title <- scores$title + 3
                                            
                                            # Author scoring
                                            if (str_detect(text, "([A-Z][a-z]+\\s+){1,}[A-Z][a-z]+")) scores$authors <- scores$authors + 2
                                            if (str_detect(text, "\\d+|@|university|department")) scores$authors <- scores$authors + 1
                                            if (y_pos > 300 & y_pos < 800 & text_length < 500) scores$authors <- scores$authors + 1
                                            
                                            # Abstract scoring
                                            if (str_detect(text_lower, "abstract|summary")) scores$abstract <- scores$abstract + 3
                                            if (text_length > 200 & text_length < 2000 & y_pos > 400) scores$abstract <- scores$abstract + 2
                                            if (str_count(text, "\\.") >= 3 & str_detect(text, "\\b(the|and|of|in|to)\\b")) scores$abstract <- scores$abstract + 1
                                            
                                            # Section header scoring
                                            if (str_detect(text, "^\\d+\\.\\s+[A-Z]|^[A-Z]{2,}\\s*$")) scores$section_header <- scores$section_header + 3
                                            if (str_detect(text_lower, "^(introduction|methods|results|discussion|conclusion)")) scores$section_header <- scores$section_header + 2
                                            
                                            # Body text scoring
                                            if (text_length > 100 & str_count(text, "\\.") >= 1) scores$body_text <- scores$body_text + 1
                                            if (y_pos > 600) scores$body_text <- scores$body_text + 1
                                            
                                            # Metadata scoring
                                            if (str_detect(text_lower, "doi|copyright|received|accepted|published")) scores$metadata <- scores$metadata + 3
                                            if (str_detect(text_lower, "©|journal|volume|issue|page")) scores$metadata <- scores$metadata + 2
                                            
                                            # Reference scoring
                                            if (str_detect(text, "^\\d+\\.\\s") & text_length < 200) scores$reference <- scores$reference + 2
                                            if (str_detect(text_lower, "et al\\.|journal|vol\\.|pp\\.")) scores$reference <- scores$reference + 1
                                            
                                            # Return classification with highest score
                                            best_class <- names(scores)[which.max(scores)]
                                            max_score <- max(unlist(scores))
                                            
                                            return(if (max_score >= 2) best_class else "unknown")
                                          },
                                          
                                          find_content_boundaries = function(all_text) {
                                            # Strategy 1: Look for explicit section headers
                                            body_start_candidates <- all_text %>%
                                              filter(is_body_start) %>%
                                              arrange(row_id)
                                            
                                            body_end_candidates <- all_text %>%
                                              filter(is_body_end) %>%
                                              arrange(row_id)
                                            
                                            # Strategy 2: Use content type classification
                                            if (nrow(body_start_candidates) == 0) {
                                              # Look for first substantial body text after front matter
                                              front_matter_end <- all_text %>%
                                                filter(content_type %in% c("front_matter", "affiliations", "metadata")) %>%
                                                arrange(desc(row_id)) %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              if (length(front_matter_end) > 0) {
                                                body_start_candidates <- all_text %>%
                                                  filter(row_id > front_matter_end, 
                                                         content_type %in% c("section_header", "body_text")) %>%
                                                  arrange(row_id) %>%
                                                  slice(1)
                                              }
                                            }
                                            
                                            if (nrow(body_end_candidates) == 0) {
                                              # Look for reference sections or sudden change to reference items
                                              ref_start <- all_text %>%
                                                filter(content_type == "reference_item") %>%
                                                arrange(row_id) %>%
                                                slice(1) %>%
                                                pull(row_id)
                                              
                                              if (length(ref_start) > 0) {
                                                body_end_candidates <- all_text %>%
                                                  filter(row_id == ref_start) %>%
                                                  slice(1)
                                              }
                                            }
                                            
                                            # Return the best boundaries found
                                            start_row <- if (nrow(body_start_candidates) > 0) body_start_candidates$row_id[1] else NULL
                                            end_row <- if (nrow(body_end_candidates) > 0) body_end_candidates$row_id[1] else NULL
                                            
                                            # Debug output
                                            message(glue("Boundary detection: start={start_row %||% 'NULL'}, end={end_row %||% 'NULL'}"))
                                            
                                            if (!is.null(start_row) && !is.null(end_row) && start_row >= end_row) {
                                              message("Warning: Start boundary after end boundary - using end boundary only")
                                              start_row <- NULL
                                            }
                                            
                                            return(list(start = start_row, end = end_row))
                                          },
                                          
                                          find_main_content_boundaries = function(content_df) {
                                            # Find where main content starts (after front matter)
                                            intro_start <- content_df %>%
                                              filter(content_category == "introduction") %>%
                                              slice(1) %>%
                                              pull(row_id)
                                            
                                            if (length(intro_start) == 0) {
                                              # Fallback: look for first substantial body text after page 1
                                              intro_start <- content_df %>%
                                                filter(page > 1 | (page == 1 & y > 1000), content_category == "body_text") %>%
                                                slice(1) %>%
                                                pull(row_id)
                                            }
                                            
                                            if (length(intro_start) == 0) {
                                              # Last resort: skip first 10% of content
                                              intro_start <- max(1, round(nrow(content_df) * 0.1))
                                            }
                                            
                                            # Find where main content ends (before references/methods details)
                                            content_end <- content_df %>%
                                              filter(content_category %in% c("reference", "reference_header", "methods_detail", 
                                                                             "publication_info", "copyright", "acknowledgments")) %>%
                                              slice(1) %>%
                                              pull(row_id)
                                            
                                            if (length(content_end) == 0) {
                                              # Fallback: use 90% of content
                                              content_end <- round(nrow(content_df) * 0.9)
                                            } else {
                                              content_end <- content_end - 1  # Stop before the end matter
                                            }
                                            
                                            return(list(start = intro_start, end = min(content_end, nrow(content_df))))
                                          },
                                          
                                          sort_by_reading_order = function(df, page_num) {
                                            if (nrow(df) == 0) return(df)
                                            
                                            message(glue("=== READING ORDER DEBUG PAGE {page_num} ==="))
                                            
                                            # Calculate center points
                                            df <- df %>%
                                              mutate(
                                                x_center = x + w/2,
                                                y_center = y + h/2,
                                                row_id = row_number()
                                              )
                                            
                                            # Show x-position distribution
                                            x_positions <- df$x_center
                                            page_width <- max(df$x_center + df$w/2, na.rm = TRUE)
                                            
                                            message(glue("Page {page_num}: {nrow(df)} regions, page width: {round(page_width)}"))
                                            message(glue("X positions range: {round(min(x_positions))} to {round(max(x_positions))}"))
                                            
                                            # Simple column detection with debugging
                                            if (length(x_positions) >= 3) {
                                              x_sorted <- sort(x_positions)
                                              gaps <- diff(x_sorted)
                                              max_gap <- max(gaps, na.rm = TRUE)
                                              threshold <- page_width * 0.15
                                              
                                              message(glue("Max gap: {round(max_gap)}, Threshold: {round(threshold)}"))
                                              
                                              if (max_gap > threshold) {
                                                # Find the boundary
                                                gap_position <- which.max(gaps)
                                                boundary <- mean(c(x_sorted[gap_position], x_sorted[gap_position + 1]))
                                                
                                                # Assign columns
                                                df <- df %>%
                                                  mutate(column = ifelse(x_center < boundary, 1, 2))
                                                
                                                # Show distribution
                                                col_counts <- df %>% count(column)
                                                message(glue("Column assignment - Col 1: {col_counts$n[1]}, Col 2: {col_counts$n[2]}"))
                                                message(glue("Boundary at x = {round(boundary)}"))
                                                
                                                # CRITICAL: Sort by column first, then y within each column
                                                result <- df %>%
                                                  arrange(column, y, x) %>%
                                                  select(-x_center, -y_center, -row_id, -column)
                                                
                                                # DEBUG: Show first few items from each column
                                                col1_items <- df %>% filter(column == 1) %>% arrange(y) %>% slice(1:3)
                                                col2_items <- df %>% filter(column == 2) %>% arrange(y) %>% slice(1:3)
                                                
                                                message("COLUMN 1 TOP ITEMS:")
                                                print(col1_items %>% mutate(preview = str_trunc(text, 50)) %>% select(y, preview))
                                                
                                                message("COLUMN 2 TOP ITEMS:")  
                                                print(col2_items %>% mutate(preview = str_trunc(text, 50)) %>% select(y, preview))
                                                
                                                message("  → Two columns detected and sorted")
                                                return(result)
                                              }
                                            }
                                            
                                            # Single column
                                            message("  → Single column detected")
                                            result <- df %>%
                                              arrange(y, x) %>%
                                              select(-x_center, -y_center, -row_id)
                                            
                                            return(result)
                                          },
                                          
                                          # And add this helper method to your R6 class as well:
                                          detect_page_layout_universal = function(df) {
                                            x_positions <- df$x_center
                                            
                                            if (length(x_positions) < 2) {
                                              return(list(columns = 1, boundary = NULL))
                                            }
                                            
                                            page_width <- max(df$x_center + df$w/2, na.rm = TRUE)
                                            
                                            # Enhanced k-means clustering
                                            tryCatch({
                                              if (length(x_positions) >= 4) {
                                                clusters <- kmeans(x_positions, centers = 2, nstart = 10)
                                                centers <- sort(clusters$centers)
                                                gap <- centers[2] - centers[1]
                                                
                                                if (gap > page_width * 0.15) {  # Lowered threshold
                                                  boundary <- mean(centers)
                                                  
                                                  # Validate distribution
                                                  left_count <- sum(x_positions < boundary)
                                                  right_count <- sum(x_positions >= boundary)
                                                  
                                                  if (left_count >= 2 && right_count >= 2) {
                                                    message(glue("  → Two columns detected: gap {round(gap)}px"))
                                                    return(list(columns = 2, boundary = boundary))
                                                  }
                                                }
                                              }
                                            }, error = function(e) {
                                              message("Column detection failed, using single column")
                                            })
                                            
                                            return(list(columns = 1, boundary = NULL))
                                          },
                                          
                                          # Debugging code (may be deleted later)
                                          debug_content_extraction = function(summary) {
                                            if (is.null(summary$raw_data)) {
                                              message("No raw data available")
                                              return(NULL)
                                            }
                                            
                                            boundaries <- summary$extraction_boundaries
                                            if (!is.null(boundaries)) {
                                              all_text <- summary$raw_data %>%
                                                filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                                arrange(page, y, x) %>%
                                                mutate(text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim())
                                              
                                              message("=== EXTRACTION BOUNDARIES DEBUG ===")
                                              message(glue("Start boundary (row {boundaries$start}):"))
                                              message(glue("  {str_trunc(all_text$text_clean[boundaries$start], 100)}"))
                                              message(glue("End boundary (row {boundaries$end}):"))
                                              message(glue("  {str_trunc(all_text$text_clean[boundaries$end], 100)}"))
                                              
                                              # Show a few rows before start
                                              if (boundaries$start > 3) {
                                                message("\nRows BEFORE start (front matter):")
                                                for (i in max(1, boundaries$start - 3):(boundaries$start - 1)) {
                                                  message(glue("  Row {i}: {str_trunc(all_text$text_clean[i], 80)}"))
                                                }
                                              }
                                              
                                              # Show a few rows after end
                                              if (boundaries$end < nrow(all_text) - 2) {
                                                message("\nRows AFTER end (references/end matter):")
                                                for (i in (boundaries$end + 1):min(nrow(all_text), boundaries$end + 3)) {
                                                  message(glue("  Row {i}: {str_trunc(all_text$text_clean[i], 80)}"))
                                                }
                                              }
                                            }
                                          },
                                          
                                          debug_reading_order = function(summary, page_num = 1) {
                                            if (is.null(summary$raw_data)) {
                                              message("No raw data available")
                                              return(NULL)
                                            }
                                            
                                            message(glue("=== DEBUGGING READING ORDER PAGE {page_num} ==="))
                                            
                                            # Get the raw data for the specified page
                                            page_data <- summary$raw_data %>%
                                              filter(page == page_num) %>%
                                              arrange(y, x)  # Default sort first
                                            
                                            message(glue("Page {page_num} has {nrow(page_data)} regions"))
                                            
                                            # Show current reading order
                                            message("CURRENT READING ORDER (first 10 items):")
                                            page_data %>%
                                              slice(1:min(10, nrow(page_data))) %>%
                                              mutate(
                                                x_info = glue("x:{x}-{x+w}"),
                                                y_info = glue("y:{y}-{y+h}"),
                                                preview = str_trunc(str_replace_all(text, "\\s+", " "), 60)
                                              ) %>%
                                              select(x_info, y_info, preview) %>%
                                              print()
                                            
                                            # Apply the sorting function and show result
                                            sorted_data <- sort_by_reading_order_universal(page_data, page_num)
                                            
                                            message("\nSORTED READING ORDER (first 10 items):")
                                            sorted_data %>%
                                              slice(1:min(10, nrow(sorted_data))) %>%
                                              mutate(
                                                x_info = glue("x:{x}-{x+w}"),
                                                y_info = glue("y:{y}-{y+h}"),
                                                preview = str_trunc(str_replace_all(text, "\\s+", " "), 60)
                                              ) %>%
                                              select(x_info, y_info, preview) %>%
                                              print()
                                            
                                            invisible(list(original = page_data, sorted = sorted_data))
                                          },
                                          
                                          debug_main_content_extraction = function(summary) {
                                            if (is.null(summary$raw_data)) {
                                              message("No raw data available")
                                              return(NULL)
                                            }
                                            
                                            message("=== DEBUGGING MAIN CONTENT EXTRACTION ===")
                                            
                                            # Show the properly sorted text with reading order
                                            all_text <- summary$raw_data %>%
                                              filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                              group_by(page) %>%
                                              group_modify(~ {
                                                page_df <- .x
                                                page_df <- page_df %>%
                                                  mutate(
                                                    x_center = x + w/2,
                                                    y_center = y + h/2,
                                                    area = w * h
                                                  )
                                                
                                                layout_info <- detect_page_layout_universal(page_df)
                                                
                                                if (layout_info$columns == 2) {
                                                  page_df <- page_df %>%
                                                    mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
                                                    arrange(column, y, x) %>%
                                                    select(-x_center, -y_center, -area, -column)
                                                } else {
                                                  page_df <- page_df %>%
                                                    arrange(y, x) %>%
                                                    select(-x_center, -y_center, -area)
                                                }
                                                
                                                return(page_df)
                                              }) %>%
                                              ungroup() %>%
                                              mutate(
                                                text_clean = str_replace_all(text, "\\s+", " ") %>% str_trim(),
                                                text_lower = tolower(text_clean),
                                                row_id = row_number()
                                              )
                                            
                                            message(glue("Total text regions after sorting: {nrow(all_text)}"))
                                            
                                            # Show first 20 regions to see the reading order
                                            message("\n=== FIRST 20 REGIONS IN READING ORDER ===")
                                            all_text %>%
                                              slice(1:min(20, nrow(all_text))) %>%
                                              mutate(
                                                preview = str_trunc(text_clean, 80),
                                                page_info = glue("Page {page}")
                                              ) %>%
                                              select(row_id, page_info, preview) %>%
                                              print()
                                            
                                            # Look for abstract content in the sorted text
                                            abstract_text <- summary$abstract_cleaned %||% summary$abstract
                                            if (!is.null(abstract_text) && nchar(abstract_text) > 50) {
                                              message("\n=== LOOKING FOR ABSTRACT BOUNDARY ===")
                                              
                                              # Get last few words of abstract for matching
                                              abstract_words <- str_split(abstract_text, "\\s+")[[1]]
                                              if (length(abstract_words) >= 10) {
                                                search_phrase <- paste(tail(abstract_words, 8), collapse = " ")
                                                message(glue("Searching for abstract ending: '...{search_phrase}'"))
                                                
                                                # Find where this appears in the sorted text
                                                for (i in 1:min(30, nrow(all_text))) {
                                                  text_to_check <- all_text$text_clean[i]
                                                  if (nchar(text_to_check) > 100) {
                                                    if (str_detect(text_to_check, fixed(search_phrase))) {
                                                      message(glue("Found abstract ending in row {i}: {str_trunc(text_to_check, 100)}"))
                                                      
                                                      # Show next few regions
                                                      message("Next few regions after abstract:")
                                                      all_text %>%
                                                        slice((i+1):min(nrow(all_text), i+8)) %>%
                                                        mutate(preview = str_trunc(text_clean, 100)) %>%
                                                        select(row_id, page, preview) %>%
                                                        print()
                                                      break
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                            
                                            # Look for introduction patterns
                                            message("\n=== LOOKING FOR INTRODUCTION START ===")
                                            intro_candidates <- all_text %>%
                                              mutate(
                                                has_intro = str_detect(text_lower, "introduction|experimental evolution|pathogenic bacterial"),
                                                starts_properly = str_detect(text_clean, "^[A-Z][a-z]"),
                                                is_substantial = nchar(text_clean) > 100,
                                                intro_score = as.numeric(has_intro) * 2 + as.numeric(starts_properly) + as.numeric(is_substantial)
                                              ) %>%
                                              filter(intro_score >= 2) %>%
                                              arrange(desc(intro_score), row_id) %>%
                                              slice(1:5)
                                            
                                            if (nrow(intro_candidates) > 0) {
                                              message("Potential introduction starts:")
                                              intro_candidates %>%
                                                mutate(preview = str_trunc(text_clean, 100)) %>%
                                                select(row_id, page, intro_score, preview) %>%
                                                print()
                                            }
                                            
                                            return(all_text)
                                          },
                                          
                                          # Cleaning methods
                                          clean_text_for_llm_aggressive = function(text) {
                                            text %>%
                                              str_replace_all('["\'°*,\\d\\s.-]{3,}(?=\\s|$)', ' ') %>%
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]+\\s+', ' ') %>%
                                              str_replace_all('([a-z])([A-Z])', '\\1 \\2') %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('\\(Fig\\.[^)]*\\)|\\(Table[^)]*\\)', '') %>%
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%
                                              str_replace_all('\\.\\s*\\.', '.') %>%
                                              str_trim()
                                          },
                                          
                                          clean_text_with_textclean_academic = function(text) {
                                            if (!requireNamespace("textclean", quietly = TRUE)) {
                                              stop("textclean package is required for this function")
                                            }
                                            
                                            cleaned <- text %>%
                                              textclean::replace_contraction() %>%
                                              textclean::replace_symbol(remove = TRUE) %>%
                                              textclean::replace_white() %>%
                                              textclean::strip() %>%
                                              textclean::replace_non_ascii(remove = TRUE)
                                            
                                            cleaned <- cleaned %>%
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{2,}\\s+(?=[a-zA-Z])', ' ') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{3,}\\s+', ' ') %>%
                                              str_replace_all('\\([Ff]ig\\.?\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('\\([Tt]able\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2') %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('(\\d+)\\s*×\\s*10\\s*([−-]?\\d+)', '\\1×10^\\2') %>%
                                              str_replace_all('(\\d+)\\s*nm', '\\1 nm') %>%
                                              str_replace_all('(\\d+)\\s*mg', '\\1 mg') %>%
                                              str_replace_all('p\\s*H', 'pH') %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_trim()
                                            
                                            return(cleaned)
                                          },
                                          
                                          clean_text_hybrid = function(text) {
                                            cleaned <- text %>%
                                              str_replace_all('\\.["\'°*,\\d\\s-]+(?=\\s+[A-Z])', '.') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{2,}\\s+(?=[a-zA-Z])', ' ') %>%
                                              str_replace_all('\\s+["\'°*,\\d\\s-]{3,}\\s+', ' ') %>%
                                              str_replace_all('\\([Ff]ig\\.?\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('\\([Tt]able\\s*\\d+[^)]*\\)', '') %>%
                                              str_replace_all('([a-z])-\\s+([a-z])', '\\1\\2')
                                            
                                            if (requireNamespace("textclean", quietly = TRUE)) {
                                              cleaned <- cleaned %>%
                                                textclean::replace_contraction() %>%
                                                textclean::replace_white() %>%
                                                textclean::replace_non_ascii(remove = TRUE)
                                            }
                                            
                                            cleaned <- cleaned %>%
                                              str_replace_all('\\s+', ' ') %>%
                                              str_replace_all('p\\s*H', 'pH') %>%
                                              str_replace_all('Ag\\s*NPs', 'AgNPs') %>%
                                              str_replace_all('Ag\\s*NP', 'AgNP') %>%
                                              str_trim()
                                            
                                            return(cleaned)
                                          },
                                          
                                          clean_title_fallback = function(text, doc_format = "generic") {
                                            cleaned <- text %>%
                                              str_replace_all("\\s+", " ") %>%
                                              str_trim()
                                            
                                            # Format-specific cleaning
                                            cleaned <- switch(doc_format,
                                                              "nature" = str_replace_all(cleaned, "^(ARTICLE|Article|DOI.*|OPEN)\\s*", ""),
                                                              "elsevier" = str_replace_all(cleaned, "^(Available online|Journal homepage).*", ""),
                                                              "mdpi" = str_replace_all(cleaned, "^(materials|nanomaterials|sustainability|Article)\\s*", ""),
                                                              "rsc" = str_replace_all(cleaned, "^(Green Chemistry|Chemical Science|Nanoscale)\\s*", ""),
                                                              str_replace_all(cleaned, "^(ARTICLE|Article|DOI.*|OPEN)\\s*", "")
                                            )
                                            
                                            return(str_trim(cleaned))
                                          },
                                          
                                          # Helper method to apply cleaning
                                          apply_cleaning_method = function(text, method) {
                                            switch(method,
                                                   "custom_aggressive" = self$clean_text_for_llm_aggressive(text),
                                                   "textclean_academic" = self$clean_text_with_textclean_academic(text),
                                                   "hybrid" = self$clean_text_hybrid(text),
                                                   text
                                            )
                                          },
                                          
                                          get_clean_results = function(summary, prefer_cleaned = TRUE) {
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
                                            
                                            authors_clean <- if (!is.null(summary$authors) && nchar(summary$authors) > 0) {
                                              # First try to split by semicolons or newlines
                                              author_list <- str_split(summary$authors, ";|\\n")[[1]] %>% 
                                                str_trim() %>% 
                                                .[. != ""] %>%
                                                .[!str_detect(., "^(Here|The|Authors)")]
                                              
                                              # If that didn't work well, try splitting by commas
                                              if (length(author_list) == 1 && str_detect(author_list[1], ",")) {
                                                author_list <- str_split(author_list[1], ",")[[1]] %>%
                                                  str_trim() %>%
                                                  .[. != ""]
                                              }
                                              
                                              # If we have authors, return them
                                              if (length(author_list) > 0 && any(nchar(author_list) > 3)) {
                                                author_list
                                              } else {
                                                "Authors not found"
                                              }
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
                                          
                                          prepare_for_claude = function(summary, body_only = FALSE, include_methods = TRUE, 
                                                                        cleaning_method = c("cleaned", "original", "best_available"),
                                                                        include_metadata = TRUE) {
                                            
                                            cleaning_method <- match.arg(cleaning_method)
                                            clean_results <- self$get_clean_results(summary)
                                            
                                            # Determine title and abstract versions
                                            if (cleaning_method == "cleaned") {
                                              title_text <- if (!is.null(summary$title_cleaned)) summary$title_cleaned else clean_results$title
                                              abstract_text <- if (!is.null(summary$abstract_cleaned)) summary$abstract_cleaned else clean_results$abstract
                                              cleaning_label <- "✓ Cleaned"
                                            } else if (cleaning_method == "original") {
                                              title_text <- clean_results$title
                                              abstract_text <- clean_results$abstract
                                              cleaning_label <- "✗ Original"
                                            } else {
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
                                            
                                            # FIXED: Choose main content version - always prefer cleaned version unless original requested
                                            if (cleaning_method == "original") {
                                              main_content <- summary$main_text_original %||% clean_results$main_text
                                              content_label <- "MAIN CONTENT (Complete Document - Original)"
                                            } else {
                                              # Use the cleaned, boundary-respecting version
                                              main_content <- summary$main_text %||% clean_results$main_text
                                              content_label <- if (body_only) {
                                                "MAIN CONTENT (Body Only - No Front Matter)"
                                              } else {
                                                "MAIN CONTENT (Cleaned)"
                                              }
                                            }
                                            
                                            # Create metadata section
                                            metadata_section <- if (include_metadata) {
                                              glue("
---
Document Statistics:
- Total Pages: {clean_results$metadata$total_pages}
- Main Text Length: {nchar(main_content)} characters
- Text Processing: {cleaning_label}
- Cleaning Method: {if(!is.null(summary$cleaning_method)) summary$cleaning_method else 'Unknown'}
- Document Format: {if(!is.null(summary$document_format)) summary$document_format else 'Unknown'}
- Column-Aware Processing: ✓ Enabled
- Processing Date: {Sys.Date()}")
                                            } else {
                                              ""
                                            }
                                            
                                            # Create the complete Claude-ready document
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
                                          
                                          get_original_text = function() {
                                            if (is.null(private$main_text_original)) {
                                              message("Original text not available. Run extract_main_text_smart first.")
                                              return(NULL)
                                            }
                                            return(private$main_text_original)
                                          },
                                          
                                          get_body_only_text = function() {
                                            if (is.null(private$main_text_body_only)) {
                                              message("Body-only text not available. Run extract_main_text_smart first.")
                                              return(NULL)
                                            }
                                            return(private$main_text_body_only)
                                          },
                                          
                                          get_content_analysis = function() {
                                            if (is.null(self$raw_data)) {
                                              message("Raw data not available. Run process_document first.")
                                              return(NULL)
                                            }
                                            
                                            # Return breakdown of content types
                                            content_breakdown <- self$raw_data %>%
                                              filter(element_type %in% c("plain text", "title") | is.na(element_type)) %>%
                                              arrange(page, y, x) %>%
                                              rowwise() %>%
                                              mutate(
                                                content_category = self$classify_content_for_extraction(text, page, y, nchar(text))
                                              ) %>%
                                              ungroup() %>%
                                              count(content_category, sort = TRUE)
                                            
                                            return(content_breakdown)
                                          }
                                          
                                        )
)


# 3. PDF PROCESSING
# Revised to keep some abandon metadata
simple_process_document <- function(pdf_path) {
  message(glue("Processing PDF: {pdf_path}"))
  
  # Setup
  use_condaenv("/home/william-ackerman/miniconda3/envs/PDF_ocr")
  cv2 <- import("cv2")
  dlyolo <- import("doclayout_yolo")
  YOLOv10 <- dlyolo$YOLOv10
  
  model <- YOLOv10("/home/william-ackerman/Desktop/MINER_DB/doclayout_yolo_docstructbench_imgsz1024.pt")
  
  # Convert PDF to images
  pdf_images <- pdf_convert(pdf_path, dpi = 300)
  message(glue("✓ Converted {length(pdf_images)} pages to images"))
  
  df_container <- list()
  layout_container <- list()
  
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
      
      py$det_res <- det_res
      py$cv2 <- cv2
      py$pdf_image <- pdf_image
      
      py_run_string("
try:
    bounding_boxes = det_res[0].summary()
    print(f'Found {len(bounding_boxes)} bounding boxes')
    
    element_types = {}
    for box in bounding_boxes:
        element_type = box['name']
        if element_type in element_types:
            element_types[element_type] += 1
        else:
            element_types[element_type] = 1
    
    print('Detected element types:', element_types)
    
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
      
      # Capture ALL element types and their layout info
      all_elements <- map_dfr(bounding_boxes, ~{
        tibble(
          page = page_num,
          element_type = .x$name,
          x = floor(.x$box$x1),
          y = floor(.x$box$y1), 
          w = floor(.x$box$x2 - .x$box$x1),
          h = floor(.x$box$y2 - .x$box$y1),
          confidence = .x$confidence,
          area = (.x$box$x2 - .x$box$x1) * (.x$box$y2 - .x$box$y1)
        )
      })
      
      layout_container[[i]] <- all_elements
      
      # ENHANCED: Include abandon elements (they contain valuable content!)
      filtered_items <- keep(bounding_boxes, ~.x$name %in% c("plain text", "title", "abandon"))
      
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
      
      # IMPORTANT: Also capture the element types for the OCR boxes
      element_types <- map_chr(filtered_items, ~.x$name)
      
      # Set up for OCR
      py$boxes <- boxes
      
      py_run_string("
import cv2
import pytesseract

image = cv2.imread(pdf_image)
results = []

for i, (x, y, w, h) in enumerate(boxes):
    try:
        roi = image[y:y+h, x:x+w]
        gray_roi = cv2.cvtColor(roi, cv2.COLOR_BGR2GRAY)
        text = pytesseract.image_to_string(gray_roi)
        
        results.append({
            'bounding_box': (x, y, w, h),
            'text': text,
            'box_index': i
        })
    except Exception as e:
        print(f'Error processing box {i}: {e}')
        continue
      ")
      
      # Convert to dataframe WITH element type information
      ocr_results <- py$results
      
      if (length(ocr_results) > 0) {
        df <- map_dfr(ocr_results, ~{
          bbox <- .x$bounding_box
          box_idx <- .x$box_index + 1
          
          tibble(
            page = page_num,
            x = bbox[[1]],
            y = bbox[[2]], 
            w = bbox[[3]],
            h = bbox[[4]],
            text = .x$text,
            element_type = if(box_idx <= length(element_types)) element_types[box_idx] else "unknown",
            area = bbox[[3]] * bbox[[4]]
          )
        }) %>%
          filter(nchar(trimws(text)) > 0) %>%
          # CRITICAL: Use abandon elements ONLY for format detection on page 1
          filter(!(element_type == "abandon" & page > 1)) %>%  # Remove abandon from other pages
          arrange(page, y, x) 
        
        df <- df %>% 
          filter(nchar(trimws(text)) > 0)
        
        # Smart column-aware sorting
        df <- sort_by_reading_order_universal(df, page_num)
        
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
    
    if (length(layout_container) > 0) {
      complete_layout <- bind_rows(layout_container)
      
      output_file <- glue("{pdf_path}.RData")
      save(complete_df, complete_layout, file = output_file)
      message(glue("✓ Results saved to {output_file} (includes layout analysis)"))
    } else {
      output_file <- glue("{pdf_path}.RData")
      save(complete_df, file = output_file)
      message(glue("✓ Results saved to {output_file}"))
    }
    
    return(complete_df)
  } else {
    message("No pages processed successfully")
    return(NULL)
  }
}

# Smart reading order function - this is the key enhancement
sort_by_reading_order_universal <- function(df, page_num) {
  if (nrow(df) == 0) return(df)
  
  message(glue("=== ENHANCED READING ORDER DEBUG PAGE {page_num} ==="))
  
  # Calculate center points
  df <- df %>%
    mutate(
      x_center = x + w/2,
      y_center = y + h/2,
      area = w * h,
      original_order = row_number()
    )
  
  # Enhanced page 1 logic for different formats
  if (page_num == 1) {
    message("  → Applying enhanced page 1 logic")
    
    # Detect format type
    is_nature_format <- any(str_detect(tolower(df$text), "nature.*nanotechnology|nature.*communications"))
    is_jac_format <- any(str_detect(tolower(df$text), "journal.*antimicrobial.*chemotherapy"))
    is_jsm_format <- any(str_detect(tolower(df$text), "jsm nanotechnology|scimed.*central"))
    
    
    if (is_nature_format) {
      message("  → Nature format detected")
      
      # Classify content by Y position and characteristics for Nature format
      df <- df %>%
        mutate(
          is_header = y < 200,  # Journal header area
          is_title_area = y >= 200 & y < 450,  # Title
          is_author_area = y >= 450 & y < 550,  # Authors
          is_boxed_abstract = y >= 550 & y < 850 & area > 30000,  # Large boxed abstract
          is_main_content = y >= 850,  # Two-column main text
          
          # Additional content detection
          is_abstract_box = str_detect(text, "Silver nanoparticles have already been successfully applied") | 
            (nchar(text) > 400 & y > 500 & y < 800),
          is_main_text_start = str_detect(text, "Silver nanoparticles \\(NPs\\) and nanocomposites are considered")
        )
      
      # Sort each section
      header_content <- df %>% filter(is_header) %>% arrange(y, x)
      title_content <- df %>% filter(is_title_area) %>% arrange(y, x)  
      author_content <- df %>% filter(is_author_area) %>% arrange(y, x)
      
      # Boxed abstract - single unit, no column logic
      boxed_abstract <- df %>% filter(is_boxed_abstract | is_abstract_box) %>% arrange(y, x)
      
      # Main content - apply column logic
      main_content <- df %>% filter(is_main_content & !is_abstract_box)
      
      if (nrow(main_content) > 0) {
        layout_info <- detect_page_layout_universal(main_content)
        
        if (layout_info$columns == 2) {
          message("  → Two-column main content detected")
          main_content <- main_content %>%
            mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
            arrange(column, y, x) %>%
            select(-column)
        } else {
          main_content <- main_content %>% arrange(y, x)
        }
      }
      
      result <- bind_rows(header_content, title_content, author_content, boxed_abstract, main_content) %>%
        select(-x_center, -y_center, -area, -original_order, 
               -is_header, -is_title_area, -is_author_area, -is_boxed_abstract, -is_main_content,
               -is_abstract_box, -is_main_text_start)
      
      message(glue("  → Nature layout: Header({nrow(header_content)}) + Title({nrow(title_content)}) + Authors({nrow(author_content)}) + Abstract({nrow(boxed_abstract)}) + Main({nrow(main_content)})"))
      
    } else if (is_jac_format) {
      message("  → JAC format detected")
      
      # JAC format has structured abstract sections
      df <- df %>%
        mutate(
          is_header_area = y < 500,  # Journal name, DOI area
          is_title_area = y >= 500 & y < 900,  # Title and authors
          is_abstract_area = y >= 900 & y < 1800,  # Abstract region
          is_main_content_area = y >= 1800,  # Keywords, Introduction start
          
          # Content type detection
          is_title = str_detect(tolower(text), "silver resistance.*gram") & nchar(text) > 50,
          is_author = str_detect(text, "([A-Z][a-z]+.*){2,}") & str_detect(text, "randall|gupta|jackson"),
          is_abstract_component = str_detect(tolower(text), "^(objectives?|methods?|results?|conclusions?):"),
          is_keywords = str_detect(tolower(text), "^keywords?:"),
          is_intro_header = str_detect(tolower(str_trim(text)), "^introduction\\s*$"),
          is_intro_content = str_detect(tolower(text), "silver cation.*ag.*centuries")
        )
      
      header_content <- df %>% filter(is_header_area) %>% arrange(y, x)
      title_content <- df %>% filter(is_title_area) %>% arrange(y, x)
      abstract_content <- df %>% filter(is_abstract_area) %>% arrange(y, x)
      main_content <- df %>% filter(is_main_content_area)
      
      # Apply column sorting to main content area
      if (nrow(main_content) > 0) {
        layout_info <- detect_page_layout_universal(main_content)
        
        if (layout_info$columns == 2) {
          main_content <- main_content %>%
            mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
            arrange(column, y, x) %>%
            select(-column)
        } else {
          main_content <- main_content %>% arrange(y, x)
        }
      }
      
      result <- bind_rows(header_content, title_content, abstract_content, main_content) %>%
        select(-x_center, -y_center, -area, -original_order, 
               -is_header_area, -is_title_area, -is_abstract_area, -is_main_content_area,
               -is_title, -is_author, -is_abstract_component, -is_keywords, 
               -is_intro_header, -is_intro_content)
      
      message(glue("  → JAC layout: Header({nrow(header_content)}) + Title({nrow(title_content)}) + Abstract({nrow(abstract_content)}) + Main({nrow(main_content)})"))
      
    
      } else if (is_jsm_format) { 
        message("  → JSM format detected")
        
        # JSM format layout classification
        df <- df %>%
          mutate(
            is_header = y < 300,  # JSM header and journal name
            is_title_area = y >= 300 & y < 700,  # Title (may span multiple lines)
            is_author_area = y >= 700 & y < 1100,  # Authors and affiliations
            is_abstract_box = y >= 1100 & y < 1600 & area > 20000,  # Boxed abstract
            is_keywords_area = y >= 1600 & y < 1800,  # Keywords
            is_main_content = y >= 1800,  # Main two-column content
            
            # Content detection
            is_abstract_content = str_detect(text, "Escherichia coli can rapidly evolve resistance"),
            is_title_content = str_detect(text, "Selection.*Silver.*Nanoparticle.*Resistance"),
            is_author_list = str_detect(text, "Mehrdad.*Tajkarimi|Kristen.*Rhinehardt") | 
              (nchar(text) > 100 & str_count(text, "[A-Z][a-z]+\\s+[A-Z][a-z]+") >= 3)
          )
        
        # Sort sections
        header_content <- df %>% filter(is_header) %>% arrange(y, x)
        title_content <- df %>% filter(is_title_area) %>% arrange(y, x)
        author_content <- df %>% filter(is_author_area) %>% arrange(y, x)
        abstract_content <- df %>% filter(is_abstract_box | is_abstract_content) %>% arrange(y, x)
        keywords_content <- df %>% filter(is_keywords_area) %>% arrange(y, x)
        
        # Main content with column detection
        main_content <- df %>% filter(is_main_content)
        
        if (nrow(main_content) > 0) {
          layout_info <- detect_page_layout_universal(main_content)
          
          if (layout_info$columns == 2) {
            main_content <- main_content %>%
              mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
              arrange(column, y, x) %>%
              select(-column)
          } else {
            main_content <- main_content %>% arrange(y, x)
          }
        }
        
        result <- bind_rows(header_content, title_content, author_content, abstract_content, keywords_content, main_content) %>%
          select(-x_center, -y_center, -area, -original_order, 
                 -is_header, -is_title_area, -is_author_area, -is_abstract_box, -is_keywords_area, -is_main_content,
                 -is_abstract_content, -is_title_content, -is_author_list)
        
        message(glue("  → JSM layout: Header({nrow(header_content)}) + Title({nrow(title_content)}) + Authors({nrow(author_content)}) + Abstract({nrow(abstract_content)}) + Keywords({nrow(keywords_content)}) + Main({nrow(main_content)})"))
      
      } else {
      
      message("  → Generic academic format")
      
      # Generic academic paper layout
      df <- df %>%
        mutate(
          is_header_area = y < 400,
          is_title_area = y >= 400 & y < 800,
          is_abstract_area = y >= 800 & y < 1500,
          is_main_content_area = y >= 1500
        )
      
      header_content <- df %>% filter(is_header_area) %>% arrange(y, x)
      title_content <- df %>% filter(is_title_area) %>% arrange(y, x)
      abstract_content <- df %>% filter(is_abstract_area) %>% arrange(y, x)
      main_content <- df %>% filter(is_main_content_area)
      
      if (nrow(main_content) > 0) {
        layout_info <- detect_page_layout_universal(main_content)
        
        if (layout_info$columns == 2) {
          main_content <- main_content %>%
            mutate(column = ifelse(x_center < layout_info$boundary, 1, 2)) %>%
            arrange(column, y, x) %>%
            select(-column)
        } else {
          main_content <- main_content %>% arrange(y, x)
        }
      }
      
      result <- bind_rows(header_content, title_content, abstract_content, main_content) %>%
        select(-x_center, -y_center, -area, -original_order, 
               -is_header_area, -is_title_area, -is_abstract_area, -is_main_content_area)
      
      message(glue("  → Generic layout: Header({nrow(header_content)}) + Title({nrow(title_content)}) + Abstract({nrow(abstract_content)}) + Main({nrow(main_content)})"))
    }
    
  } else {
    # Regular column detection for pages 2+
    layout_info <- detect_page_layout_universal(df)
    
    if (layout_info$columns == 2) {
      message(glue("  → Page {page_num}: Two columns, boundary at x = {round(layout_info$boundary)}"))
      
      col1_sorted <- df %>%
        filter(x_center < layout_info$boundary) %>%
        arrange(y, x)
      
      col2_sorted <- df %>%
        filter(x_center >= layout_info$boundary) %>%
        arrange(y, x)
      
      result <- bind_rows(col1_sorted, col2_sorted) %>%
        select(-x_center, -y_center, -area, -original_order)
      
    } else {
      message(glue("  → Page {page_num}: Single column"))
      result <- df %>%
        arrange(y, x) %>%
        select(-x_center, -y_center, -area, -original_order)
    }
  }
  
  return(result)
}

detect_page_layout_simple = function(df) {
  x_positions <- df$x_center
  
  if (length(x_positions) < 3) {
    return(list(columns = 1, boundary = NULL))
  }
  
  # Simple gap-based detection
  page_width <- max(df$x_center + df$w/2, na.rm = TRUE)
  
  # Find the largest gap in x-positions
  x_sorted <- sort(x_positions)
  gaps <- diff(x_sorted)
  max_gap <- max(gaps, na.rm = TRUE)
  
  # If there's a significant gap, assume 2 columns
  if (max_gap > page_width * 0.2) {
    gap_position <- which.max(gaps)
    boundary <- mean(c(x_sorted[gap_position], x_sorted[gap_position + 1]))
    return(list(columns = 2, boundary = boundary))
  }
  
  return(list(columns = 1, boundary = NULL))
}

# Enhanced layout detection with academic paper heuristics
detect_page_layout <- function(df) {
  x_positions <- df$x_center
  areas <- df$area
  
  if (length(x_positions) < 2) {
    return(list(columns = 1, boundary = NULL, boundaries = NULL))
  }
  
  # Nature Communications specific column detection
  # Typical layout: left column ~300-500px, right column ~600-800px (approximate)
  page_width <- max(df$x_right, na.rm = TRUE)
  
  # Filter main text (exclude small elements like page numbers, headers)
  main_text_threshold <- quantile(areas, 0.3)
  main_text_boxes <- df %>%
    filter(area >= main_text_threshold) %>%
    pull(x_center)
  
  if (length(main_text_boxes) < 2) {
    return(list(columns = 1, boundary = NULL, boundaries = NULL))
  }
  
  # Use k-means clustering to identify column centers
  if (length(main_text_boxes) >= 4) {
    tryCatch({
      clusters <- kmeans(main_text_boxes, centers = 2)
      centers <- sort(clusters$centers)
      
      # Check if clusters are well-separated (Nature format typically has wide gap)
      gap <- centers[2] - centers[1]
      if (gap > page_width * 0.2) {  # At least 20% of page width gap
        boundary <- mean(centers)
        message(glue("  → Two columns detected: Left (~{round(centers[1])}), Right (~{round(centers[2])}), Gap: {round(gap)}px"))
        return(list(columns = 2, boundary = boundary, boundaries = c(boundary)))
      }
    }, error = function(e) {
      message("K-means clustering failed, falling back to simple detection")
    })
  }
  
  # Fallback: simple gap detection
  x_sorted <- sort(main_text_boxes)
  gaps <- diff(x_sorted)
  max_gap_idx <- which.max(gaps)
  max_gap <- gaps[max_gap_idx]
  
  if (max_gap > page_width * 0.15) {  # 15% of page width
    boundary <- mean(c(x_sorted[max_gap_idx], x_sorted[max_gap_idx + 1]))
    return(list(columns = 2, boundary = boundary, boundaries = c(boundary)))
  }
  
  return(list(columns = 1, boundary = NULL, boundaries = NULL))
}

detect_page_layout_universal <- function(df) {
  
  # Add NA filtering at the start
  df <- df %>%
    filter(!is.na(x_center), !is.na(area), !is.na(text))
  
  x_positions <- df$x_center
  areas <- df$area
  
  if (length(x_positions) < 3) {
    message("  → Insufficient regions for column detection")
    return(list(columns = 1, boundary = NULL))
  }
  
  # Get page dimensions
  page_width <- max(df$x + df$w, na.rm = TRUE)
  page_left <- min(df$x, na.rm = TRUE)
  page_right <- max(df$x + df$w, na.rm = TRUE)
  
  # Filter to substantial text blocks for better column detection
  substantial_df <- df %>%
    filter(
      nchar(str_trim(text)) > 20,  # Minimum text length
      w * h > 1000,  # Minimum area
      !str_detect(tolower(text), "^(page|doi|figure|table)\\s")  # Exclude headers/captions
    )
  
  if (nrow(substantial_df) < 3) {
    message("  → Insufficient substantial text for column detection")
    return(list(columns = 1, boundary = NULL))
  }
  
  message(glue("  → Page width: {page_width}, analyzing {nrow(substantial_df)} substantial regions"))
  
  # Method 1: K-means clustering on substantial text
  substantial_x <- substantial_df$x_center
  
  tryCatch({
    if (length(substantial_x) >= 4) {
      clusters <- kmeans(substantial_x, centers = 2, nstart = 20)
      centers <- sort(clusters$centers)
      gap <- centers[2] - centers[1]
      
      message(glue("  → K-means centers: {round(centers[1])}, {round(centers[2])}, gap: {round(gap)}"))
      
      # Check if gap is significant and clusters are well-separated
      if (gap > page_width * 0.12 && clusters$betweenss/clusters$totss > 0.3) {
        boundary <- mean(centers)
        
        # Validate cluster assignment
        left_count <- sum(substantial_x < boundary)
        right_count <- sum(substantial_x >= boundary)
        
        if (left_count >= 1 && right_count >= 1) {
          message(glue("  → Two columns detected via K-means, boundary: {round(boundary)}"))
          message(glue("  → Left cluster: {left_count} regions, Right cluster: {right_count} regions"))
          return(list(columns = 2, boundary = boundary))
        }
      }
    }
  }, error = function(e) {
    message(glue("  → K-means failed: {e$message}"))
  })
  
  # Method 2: Gap-based detection
  x_sorted <- sort(unique(round(substantial_x/10) * 10))  # Round to 10px for grouping
  if (length(x_sorted) >= 2) {
    gaps <- diff(x_sorted)
    max_gap <- max(gaps)
    
    if (max_gap > page_width * 0.15) {
      gap_idx <- which.max(gaps)
      boundary <- mean(c(x_sorted[gap_idx], x_sorted[gap_idx + 1]))
      
      message(glue("  → Two columns detected via gap analysis, boundary: {round(boundary)}"))
      message(glue("  → Max gap: {max_gap} at position {round(boundary)}"))
      return(list(columns = 2, boundary = boundary))
    }
  }
  
  message("  → Single column layout detected")
  return(list(columns = 1, boundary = NULL))
}

classify_content_universal = function(text, text_length, y_pos, element_type = "unknown") {
  text_lower <- tolower(text)
  
  # Quick classification based on obvious patterns
  if (element_type == "title") return("title")
  if (element_type == "figure_caption") return("caption") 
  if (element_type == "abandon") return("metadata")
  
  # Title detection
  if (y_pos < 500 && text_length > 20 && text_length < 300 && 
      str_detect(text, "^[A-Z]") && !str_detect(text_lower, "^(journal|article|doi)")) {
    return("title")
  }
  
  # Author detection  
  if (str_detect(text, "([A-Z][a-z]+\\s+){1,}[A-Z][a-z]+") && 
      y_pos > 300 && y_pos < 800 && text_length < 500) {
    return("authors")
  }
  
  # Abstract detection
  if ((str_detect(text_lower, "abstract|summary") || 
       (text_length > 200 && text_length < 2000 && y_pos > 400 && str_count(text, "\\.") >= 3))) {
    return("abstract")
  }
  
  # Section header
  if (str_detect(text, "^\\d+\\.\\s+[A-Z]|^[A-Z]{2,}\\s*$") || 
      str_detect(text_lower, "^(introduction|methods|results|discussion|conclusion)")) {
    return("section_header")
  }
  
  # Metadata
  if (str_detect(text_lower, "doi|copyright|received|accepted|published|©|journal|volume")) {
    return("metadata")
  }
  
  # Default to body text for substantial content
  if (text_length > 50 && str_count(text, "\\.") >= 1) {
    return("body_text")
  }
  
  return("unknown")
}


options(max.print = 50000)  

# RUN THE CODE
setwd("/home/william-ackerman/Desktop/TEST_AGAIN")
list.files(pattern = "pdf")

pdf <- "Impeding Biofilm-Forming Mediated Methicillin-Resistant Staphylococcus aureus and Virulence Genes Using a Biosynthesized Silver Nanoparticles–Antibiotic Combination.pdf"

# Process with improved column handling
processor <- GenericDocumentProcessor$new()
summary <- processor$process_document(pdf, model = "llama3.1", 
                                      cleaning_method = "hybrid")

# Use cleaned versions (default)
claude_text_cleaned <- processor$prepare_for_claude(summary,  
                                                    cleaning_method = "cleaned", 
                                                    include_metadata = TRUE)

c