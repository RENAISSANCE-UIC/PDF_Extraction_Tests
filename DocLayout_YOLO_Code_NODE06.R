# PDF to image
pacman::p_load(
  magick,
  pdftools
)

Sys.setenv(TESSDATA_PREFIX = "/usr/share/tesseract-ocr/5/tessdata")

setwd("/home/weackerm/Desktop/PDF_Extractor")
pdf_file <- "25467683.pdf" # Example PDF

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

model <- 
  YOLOv10("doclayout_yolo_docstructbench_imgsz1024.pt")

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

# Pull Title
complete_df %>% filter(page == 1) %>% 
  arrange(y) %>% filter(y < 900)  %>% 
  select(text) %>% unlist() %>% unname() -> title

# Pull abstract
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% filter(x > 800) %>% 
  filter(x < 1200) %>% slice(4) %>% select(text) %>% 
  unlist() %>% unname() -> abstract

# Pull Intro column 1
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% filter(x < 1200 & y > 2600) %>% 
  select(text) %>% unlist() %>% unname() -> col1

# Pull columnn 2 
complete_df  %>% filter(page == 1) %>% 
  arrange(y) %>% filter(x > 1200 & y > 2600)  %>% 
  select(text) %>% unlist() %>% unname() -> col2

combined_text_p1 <- 
  paste(paste(col1, collapse = ""), 
        paste(col2, collapse = ""),
        sep = "")


complete_df  %>% filter(page == 2) %>% 
  arrange(y) %>% filter(x < 1200) %>% 
  select(text) %>% unlist() %>% unname() -> col1

# Pull columnn 2 
complete_df  %>% filter(page == 2) %>% 
  arrange(y) %>% filter(x > 1200)  %>% 
  select(text) %>% unlist() %>% unname() -> col2

# Note that pdftools can extract the data directly but it contains 
# additional information
full_ms_text <- pdf_text(pdf_file)
glue::glue(full_ms_text[2]) # Glue sets it in human readable format

#str_replace_all(full_ms_text, "\\n\\s*", " ")



# Is there some way to fuzzy match OCR text with actual extracted text to 
# improve accuracy/error correct?
# full_ms_text[1]
# 
# adist <- stringdist::afind(glue::glue(full_ms_text[2]), 
#                            df_container[[2]]$text[1] )
# substr(full_ms_text[1], 
#        adist$location, 
#        adist$location + nchar(df_container[[1]]$text[1]))


