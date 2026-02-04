# it works on my machine
library(tuber)
## !!
setwd("/home/me/Code/scrapeYT/")
source("./youtubeoauth.R")

## make sure to authenticate before use
## oauth should look like this:
# app_id          <- "<foo>.apps.googleusercontent.com"
# app_secret      <- "<fooooo>"
# yt_oauth(app_id, app_secret, token = '')

# vid: video id unique to each YouTube video

savecomments <- function(vid, create_csv = F, create_HTML = T){
  
  comments <- get_all_comments(video_id = vid)
  
  timecode <- format(Sys.time(), "%Y-%m-%d_%H-%M-%OS")  # format time
  fname    <- paste0("data/", vid, "_", timecode, "_comments.csv")  # set filename
  
  if (create_csv) {
    if(!dir.exists("data")) dir.create("data")
    write.csv(comments, file = fname)
    cat("CSV file generated: ", fname, "\n")
  }
  
  if (create_HTML) {
    if(!dir.exists("html")) dir.create("html")
    
    # minimal subset of comments data
    content <- data.frame(
      id       = comments$id,
      time     = as.POSIXct(comments$publishedAt, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      parentid = comments$parentId,
      author   = comments$authorDisplayName,
      picture  = comments$authorProfileImageUrl,
      text     = comments$textDisplay
    )
    
    # escape HTML special characters
    escape_html <- function(text) {
      text <- gsub("&", "&amp;", text)
      text <- gsub("<", "&lt;", text)
      text <- gsub(">", "&gt;", text)
      text <- gsub('"', "&quot;", text)
      text <- gsub("'", "&#39;", text)
      return(text)
    }
    
    # sort content by datetime
    content <- content[order(content$time), ]
    
    # video details
    videodata   <- get_video_details(video_id = vid)
    videotitle  <- videodata$items[[1]]$snippet$title
    channelname <- videodata$items[[1]]$snippet$channelTitle
    channelid   <- videodata$items[[1]]$snippet$channelId
    
    channelurl <- paste0("https://www.youtube.com/channel/", channelid)
    videourl   <- paste0("https://youtu.be/", vid)
    
    cat("Video title:", videotitle, "\n")
    cat("Channel name:", channelname, "\n")
    cat("Channel URL:", channelurl, "\n")
    cat("Video URL:", videourl, "\n")
    
    # HTML skeleton
    html_lines <- c(
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<meta charset='UTF-8'>",
      "<title>YouTube Comments</title>",
      "<style>",
      "body { font-family: Arial, sans-serif; line-height: 1.4; padding: 20px; background-color: #f8f8f8; }",
      ".comment { border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; background-color: #fff; border-radius: 8px; }",
      ".children { margin-left: 40px; margin-top: 5px; }",
      ".author { font-weight: bold; }",
      ".timestamp { color: #666; font-size: 0.85em; }",
      ".text { margin-top: 5px; }",
      ".profile-pic { width: 32px; height: 32px; border-radius: 50%; vertical-align: middle; margin-right: 10px; }",
      "</style>",
      "</head>",
      "<body>",
      paste0("<h1><a href='", videourl, "' target='_blank'>", escape_html(videotitle), "</a></h1>"),
      paste0("<h2>by <a href='", channelurl, "' target='_blank'>", escape_html(channelname), "</a></h2>")
    )
    
    # Function to generate HTML for a single comment recursively
    # Function to generate HTML for a single comment recursively
    render_comment <- function(comment_row) {
      author_link <- paste0(
        "<a href='https://www.youtube.com/", comment_row$author,
        "' target='_blank' class='author'>", comment_row$author, "</a>"
      )
      
      html <- c(
        "<div class='comment'>",
        paste0("<img src='", comment_row$picture, "' class='profile-pic'>"),
        paste0(author_link, " "),
        paste0("<span class='timestamp'>", format(comment_row$time, "%Y-%m-%d %H:%M:%S"), "</span>"),
        paste0("<div class='text'>", comment_row$text, "</div>")  # <-- keep raw HTML here
      )
      
      # render children recursively
      child_rows <- content[!is.na(content$parentid) & content$parentid == comment_row$id, ]
      if(nrow(child_rows) > 0) {
        child_rows <- child_rows[order(child_rows$time), ]
        child_html <- unlist(lapply(1:nrow(child_rows), function(i) render_comment(child_rows[i, ])))
        html <- c(html, "<div class='children'>", child_html, "</div>")
      }
      
      html <- c(html, "</div>")
      return(html)
    }
    
    
    # Render top-level comments
    parents <- content[is.na(content$parentid), ]
    for(i in 1:nrow(parents)) {
      html_lines <- c(html_lines, render_comment(parents[i, ]))
    }
    
    html_lines <- c(html_lines, "</body>", "</html>")
    
    hname <- paste0("html/", vid, "_", timecode, "_comments.html")
    writeLines(html_lines, con = hname)
    cat("HTML file generated: ", hname, "\n")
  }
  
  return(list(
    comments = comments,
    content  = content,
    csv_file = if(create_csv) fname else NULL,
    html_file = if(create_HTML) hname else NULL
  ))
}
