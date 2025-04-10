# PDF to image
pacman::p_load(
  magick,
  pdftools
)

Sys.setenv(TESSDATA_PREFIX = "/usr/share/tesseract-ocr/5/tessdata")

setwd("/home/weackerm/Desktop/PDF_Extractor/pdfs_papers/Shapes_Papers_Batch1")
files <- list.files(pattern = ".pdf$")

pdf_file <- "25467683.pdf" # Example PDF
pdf_file <- files[6]

pdf_images <- pdf_convert(pdf_file, dpi = 300)

pacman::p_load(
  tidyverse,
  reticulate,
  ellmer
)

use_condaenv("PDF_xtract")

cv2 <- import("cv2")
dlyolo <- import("doclayout_yolo")
YOLOv10 <- dlyolo$YOLOv10

# cv2_version <- cv2$`__version__`
# print(cv2_version)
# 
# dlyolo$`__version__`
# 
# tess <- import("pytesseract")
# tess$`__version__`

model <- 
  YOLOv10("/home/weackerm/Desktop/PDF_Extractor/doclayout_yolo_docstructbench_imgsz1024.pt")

df_container <- list()

for(i in 1:length(pdf_images)) {
  pdf_image <- pdf_images[i]
  page_num <- i
  message("Performing predictions with Doclayout YOLO model")
  
  det_res <- model$predict(
    pdf_image,   # Image to predict
    imgsz=as.integer(1024),        # Prediction image size
    conf=0.2,          # Confidence threshold
    device="cuda:0"    # Device to use (e.g., 'cuda:0' or 'cpu')
  )
  
  py_run_string("
bounding_boxes = r.det_res[0].summary()
  ")
  
  img_name <- str_split(pdf_image, "\\.")[[1]][1]
  res_name <- paste0("prediction_", img_name, ".jpg")
  
  py_run_string("
  # Annotate and save result
annotated_frame = r.det_res[0].plot(pil=True, line_width=5, font_size=20)
r.cv2.imwrite(r.res_name, annotated_frame)
  ")
  
  bounding_boxes <- py$bounding_boxes
  
  filtered_items <- lapply(bounding_boxes, 
                           function(x) if (x$name %in% c("plain text", "title")) x else NULL) %>% 
    Filter(Negate(is.null), .)# Filter to only plain text boxes
  
  boxes <-
    lapply(filtered_items,
           function(x) {
             x1 = floor(x$box$x1) %>% as.integer()
             y1 = floor(x$box$y1) %>% as.integer()
             w = floor(x$box$x2 - x$box$x1 ) %>% as.integer()
             h = floor(x$box$y2 - x$box$y1) %>% as.integer()
             list(x1,y1,w,h)
           })
  
  message("Extracting text with tesseract")
  
  py_run_string("
import cv2
import pytesseract
  
image = cv2.imread(r.pdf_image)
  
# Initialize a list to store the results
results = []
  
for (x, y, w, h) in r.boxes:
  roi = image[y:y+h, x:x+w]  # Extract region of interest
  gray_roi = cv2.cvtColor(roi, cv2.COLOR_BGR2GRAY)  # Convert to grayscale
  text = pytesseract.image_to_string(gray_roi)  # Apply OCR
    
  # Store the result in a dictionary
  result = {
        'bounding_box': (x, y, w, h),
        'text': text
  }
  
  results.append(result)
  ")
  
  # Function to extract bounding box and text
  extract_info <- function(x) {
    data.frame(
      x = x$bounding_box[[1]],
      y = x$bounding_box[[2]],
      w = x$bounding_box[[3]],
      h = x$bounding_box[[4]],
      text = x$text
    )
  }
  
  df <- do.call(rbind, lapply(py$results, extract_info))
  df$page <- page_num 
  df <- df %>% filter(!duplicated(df$text))
  
  df <- df[,c("page", "x", "y", "w", "h", "text")]
  
  message("Collating dataframe.")
  
  df_container[[i]] <- as_tibble(df) %>% arrange(y)
}

complete_df <- do.call(rbind, df_container)

filename <- glue::glue("{pdf_file}.RData")

save(complete_df, file = filename)

# Pull Title
complete_df %>% filter(page == 1) %>% 
  arrange(y) %>% slice(1) %>% 
  select(text) %>% unlist() %>% unname() -> title

# Pull abstract
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% slice(7) %>% select(text) %>% 
  unlist() %>% unname() -> abstract

# Two column abstract 
{
complete_df  %>% filter(page == 1) %>% 
  slice(3:7) %>% 
  arrange(y) %>% filter(x < 1180) %>% 
  select(text) %>% unlist() %>% unname() -> col1.a

# Pull column 2 
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% filter(x > 1180)  %>% 
  slice(1) %>% 
  select(text) %>% unlist() %>% unname() -> col2.a

abstract <- 
  paste(paste(col1.a, collapse = ""), 
        paste(col2.a, collapse = ""),
        sep = "")

}
# complete_df  %>% filter(page == 1) %>% 
#   arrange(y) %>% filter(x > 1180)  %>% 
#   slice(3:4) %>% 
#   select(text) %>% unlist() %>% unname() %>% 
#   paste(., collapse = "") -> combined_text_p1

# Pull Intro column 2
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% filter(x < 1200) %>% slice(10)  %>% 
  select(text) %>% unlist() %>% unname() -> col1.1

# Pull columnn 2 
complete_df  %>% filter(page == 1) %>% 
  arrange(y )%>% filter(x > 1200)  %>% slice(1:2) %>% 
  select(text) %>% unlist() %>% unname() -> col2.1

combined_text_p1 <- 
  paste(paste(col1.1, collapse = ""), 
        paste(col2.1, collapse = ""),
        sep = "")

# Pull page 2 
#p_num =2

pull_page_single_column <- function(complete_df, page_num){ 
  complete_df %>% filter(page == page_num) %>% 
    arrange(y) %>% select(text) %>% unlist() %>% unname() -> text
  return(text)
}


pull_page_double_column <- function(complete_df, p_num){
  complete_df  %>% filter(page == p_num) %>% 
    arrange(y) %>% filter(x < 1200) %>% 
    select(text) %>% unlist() %>% unname() -> col1x
  
  complete_df  %>% filter(page == p_num) %>% 
    arrange(y) %>% filter(x > 1200)  %>% 
    select(text) %>% unlist() %>% unname() -> col2x
  
  combined_text_p <- 
    paste(paste(col1x, collapse = ""), 
          paste(col2x, collapse = ""),
          sep = "")
  
  return(combined_text_p)
}

combined_text_p2  <- pull_page_double_column(complete_df, 2)
combined_text_p3  <- pull_page_double_column(complete_df, 3)
combined_text_p4  <- pull_page_double_column(complete_df, 4)
combined_text_p5  <- pull_page_double_column(complete_df, 5)
combined_text_p6  <- pull_page_double_column(complete_df, 6)
combined_text_p7  <- pull_page_double_column(complete_df, 7)
combined_text_p8  <- pull_page_double_column(complete_df, 8)

# Pull final page
complete_df  %>% filter(page == 9) %>% 
  arrange(y) %>% filter(x < 1200) %>% 
  slice(1:4) %>% select(text) %>% unlist() %>% unname() -> col1.F

complete_df  %>% filter(page == 9) %>% 
  arrange(y) %>% filter(x > 1200)  %>% 
  slice(1) %>% select(text) %>% unlist() %>% unname() -> col2.F <- NULL

combined_text_pF <- 
  paste(paste(col1.F, collapse = ""), 
        paste(col2.F, collapse = ""),
        sep = "")

text <- list()
text$title <- title
text$abstract <- abstract
text$page1 <- combined_text_p1
text$page2 <- combined_text_p2
text$page3 <- combined_text_p3
text$page4 <- combined_text_p4
text$page5 <- combined_text_p5
text$page6 <- combined_text_p6
text$page7 <- combined_text_p7
text$page8 <- combined_text_p8
text$page9 <- combined_text_pF 

name2 <- glue::glue("{pdf_file}_Abstracted.RData")

save(text, file = name2)


# Note that pdftools can extract the data directly but it contains 
# additional information
full_ms_text <- pdf_text(pdf_file)
glue::glue(full_ms_text[1]) # Glue sets it in human readable format

# Summarize extracted text
Sys.setenv(ANTHROPIC_API_KEY = '<KEY>')

# System prompt for LLM
prompt <- "I am providing you with a Pubmed article. 
I need a structured summary.
Tell me what nanoparticles are being used.
Tell me what the nanoparticles are made of.
Tell me what the sizes of the nanoparticles if, if reported, 
or provide a size range, if reported.
Tell me if different shapes of the nanoparticles are used.
Tell me what concentrations are used.
Tell me what microorganisms are the nanoparticles are being tested 
against, if reported.
Tell me about methods of nanoparticle characterization.
Tell me about the context of the study and any relevant notes."
prompt <- gsub("\\n", " ", x=prompt)

# System prompt for claude-sonnet
prompt2 <- 
  "I am providing you with structured summaries of a scientific paper.
The papers are about nanoparticles (NPs) and antimicrobial activities.  
Each structured summary has information on the NPs being 
used, the composition of the nanoparticles, the sizes of the nanoparticles, the 
concentrations used, the shapes of the nanoparticles (if any), the antimicrobial 
activity, and the types of microorganisms they were used on.
Make a summary table of these results. Use columns: 
NP_Class: e.g., Metal, Metal Oxide, Metal Salt
MMO-NP: e.g., CdO-NP, Cu-NP. Au-NP. Ag-NP	
Avg._Diameter_(nm): e.g., 22, 9.25, 1.8	
Shape: e.g., Spherical, Nanoflakes, Cuboidal, N/A
Composition: e.g., MoS2, Co, Ag, C
Coating: e.g., Polyethylene glycol/Alginate, Thioglycolic acid
Antibiotic_bound_to_NP: e.g., tobramycin, vancomycin
Microorganism: e.g., P. aeruginosa, S. aureus
Microorganism_Detail: e.g., Staphylococcus aureus (ATCC 9144), Escherichia coli (ATCC 10536)
Pseudomonas aeruginosa (ATCC 9027/ATCC 97)
Treatment_Time_(h): e.g., 24, 18-20, 2	
Antimicrobial_Activity_Measurement: e.g,. MIC (μg/ml), IC50 (μg/ml), MBC (μg/ml), PPM
Antimicrobial_Activity_Value: e.g., 64, 6.75, 27
"
prompt2 <- gsub("\\n", " ", x=prompt2)

model1="llama3.1"
model2="claude-3-opus-20240229"
model3="claude-3-7-sonnet-20250219"

chat_anthropic <- chat_claude(system_prompt = prompt, model=model3)

flat_text <- paste(unlist(text), collapse = "\n")
flat_text <- glue::glue(paste(unlist(text), collapse = "\n"))

chat_anthropic$chat(flat_text)

to_extract <- list.files(pattern = ".md$")
input_text <- readLines(to_extract[6])
chat_anthropic2 <- chat_claude(system_prompt = prompt2, model=model3)

claude_res <- chat_anthropic2$chat(input_text)

table_text <- sub(".*\\| PMID \\|", "| PMID |", claude_res) 

table_lines <- strsplit(table_text, "\n")[[1]]
#table_lines <- table_lines[3:7]
table_df <- read.delim(text = paste(table_lines, collapse = "\n"), 
                       sep = "|", header = TRUE, strip.white = TRUE)

write.csv(table_df, "DF_SUMMARY_38726825.csv")
