# Individual Assignment 1 Jump-Start Code (Word Clouds Only)

windows_system <- TRUE  # setting for Windows
setwd("C:/Users/crmo/Assignment1_Coolidge/455_wordcloud_jump_start_v001")

library(wordcloud)  # bring wordcloud package into workspace
library(tm)  # bring text analytics tools into workspace

if (windows_system)
    directory_location <- paste(getwd(), "\\text_documents\\", sep = "")

if(!windows_system)     
    directory_location <- paste(getwd(), "/text_documents/", sep = "")

file_names <- dir(directory_location)

text_data_frame <- NULL
for (ifile in seq(along = file_names)) {

    this_file <- paste(paste(directory_location, file_names[ifile], sep = ""))

    this_text <- tolower(scan(this_file, what = "char", sep = "\n"))

    this_text_words <- unlist(strsplit(this_text, "\\W"))

    this_text_vector <- this_text_words[which(nchar(this_text_words) > 0)]

    this_data_frame <- data.frame(document = file_names[ifile], text = this_text_vector,
        stringsAsFactors = FALSE)
    text_data_frame <- rbind(text_data_frame, this_data_frame)
    }

print(str(text_data_frame))

# -----------------------------------
# word clouds
# -----------------------------------

for (ifile in seq(along = file_names)) {
    this_file_label <- strsplit(file_names[ifile], ".txt", "")[[1]]

    pdf(file = paste("word_cloud_", this_file_label, ".pdf", sep = ""),
        width = 8.5, height = 8.5)
    cat("\nPlotting ", this_file_label)    
    this_text_vector <- as.character(subset(text_data_frame,
        subset = (document == file_names[ifile]), select = text))

    wordcloud(this_text_vector, min.freq = 5,
        max.words = 150, 
        random.order = FALSE,
        random.color = FALSE,
        rot.per = 0.0,
        colors = brewer.pal(6, "Dark2"),
        ordered.colors = FALSE,
        use.r.layout = FALSE,
        fixed.asp = TRUE)
    dev.off()
    }  
cat("\n\n RUN COMPLETE")