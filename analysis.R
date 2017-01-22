setwd("C:/Users/Antoine/Desktop/streamroot/")

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(data.table)
library(ggplot2)

dataset <- fread("data.csv")

study.dataset <- function(dataset) 
{
  dt <- dataset[!is.na(cdn), ]
  dt[, `#stream` := as.character(`#stream`)]
  
  dt <- dt[, .(Views = .N), by = list(`#stream`, isp, browser)]
  
  dt1 <- dt[, .(Views = sum(Views)), by = list(browser, isp)]
  dt2 <- dt[, .(Views = sum(Views)), by = list(`#stream`, isp)]
  dt3 <- dt[, .(Views = sum(Views)), by = list(browser, `#stream`)]
  
  p01 <- ggplot(dt1, aes(x = browser, y = isp, label = Views)) + geom_raster(aes(fill = Views)) + geom_text(colour = "red")
  p02 <- ggplot(dt2, aes(x = `#stream`, y = isp, label = Views)) + geom_raster(aes(fill = Views)) + geom_text(colour = "red")
  p03 <- ggplot(dt3, aes(x = browser, y = `#stream`, label = Views)) + geom_raster(aes(fill = Views)) + geom_text(colour = "red")

  p01 <- p01 + ggtitle("Repartiton of the dataset rows function of the browser used and the isp")
  p02 <- p02 + ggtitle("Repartiton of the dataset rows function of the video and the isp")
  p03 <- p03 + ggtitle("Repartiton of the dataset rows function of the browser used and video")

  dir.create("plots", showWarnings = FALSE)
  dir.create("plots/dataset repartition", showWarnings = FALSE)
  
  ggsave('plots/dataset repartition/views_repartition_browser_isp.png', p01, dpi = 1000)
  ggsave('plots/dataset repartition/views_repartition_stream_isp.png', p02, dpi = 1000)
  ggsave('plots/dataset repartition/views_repartition_browser_stream.png', p03, dpi = 1000)
}

study.video.sizes <- function(dataset) 
{
  videos <- dataset[connected == "false", .(CDN = mean(cdn), CDN.range = (max(cdn) - min(cdn)) / mean(cdn)), by = `#stream`]
  print(videos)
}


study.connectivity <- function(dataset)
{
  dt <- dataset[!is.na(cdn), .(`#stream` = as.character(`#stream`), isp, browser, connected = (connected == "true"))]
  
  dt.by.stream <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = `#stream`]
  dt.by.isp <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = isp]
  dt.by.browser <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = browser]
  
  dt.stream.isp <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = list(`#stream`, isp)]
  dt.stream.browser <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = list(`#stream`, browser)]
  dt.isp.browser <- dt[, .(n.connected = sum(connected), n.not.connected = .N - sum(connected), connection.ratio = sum(connected) / .N), by = list(browser, isp)]
  
  p.stream <- ggplot(dt.by.stream, aes(`#stream`, connection.ratio)) + geom_bar(colour = "blue", stat = "identity")
  p.isp <- ggplot(dt.by.isp, aes(x = isp, y = connection.ratio)) + geom_bar(colour = "blue", stat = "identity")
  p.browser <- ggplot(dt.by.browser, aes(x = browser, y = connection.ratio)) + geom_bar(colour = "blue", stat = "identity")
  
  p.stream.isp <- ggplot(dt.stream.isp, aes(x = `#stream`, y = isp, label = round(connection.ratio, 3))) + geom_raster(aes(fill = connection.ratio)) + geom_text(colour = "red")
  p.stream.browser <- ggplot(dt.stream.browser, aes(x = `#stream`, y = browser, label = round(connection.ratio, 3))) + geom_raster(aes(fill = connection.ratio)) + geom_text(colour = "red")
  p.isp.browser <- ggplot(dt.isp.browser, aes(x = isp, y = browser, label = round(connection.ratio, 3))) + geom_raster(aes(fill = connection.ratio)) + geom_text(colour = "red")
  
  dir.create("plots/connectivity", showWarnings = FALSE)

  ggsave('plots/connectivity/connection_1_ratio_stream.png', p.stream, dpi = 600)
  ggsave('plots/connectivity/connection_1_ratio_isp.png', p.isp, dpi = 600)
  ggsave('plots/connectivity/connection_1_ratio_browser.png', p.browser, dpi = 600)
  
  ggsave('plots/connectivity/connection_2_stream_isp.png', p.stream.isp, dpi = 600)
  ggsave('plots/connectivity/connection_2_stream_browser.png', p.stream.browser, dpi = 600)
  ggsave('plots/connectivity/connection_2_isp_browser.png', p.isp.browser, dpi = 600)
  
  cat("\nRepartition of the different isp for #stream 7 :\n")
  print(table(dt[`#stream` == 7, isp]))
  
  cat("\nRepartition of the different stream for Datch Telecom :\n")
  print(table(dt[isp == "Datch Telecam", `#stream`]))
  cat("\n")
  
  return()
}


study.p2p.ratio.v0 <- function(dataset)
{
  dt <- dataset[!is.na(cdn) & connected == "true", ]
  dt[, p2p.ratio := (p2p / (cdn + p2p))]
  
  add_sizes <- function(x) {
    if(x == 1) return("1 (~300Ko)")
    if(x == 4) return("4 (~100Mo)")
    if(x == 8) return("8 (~100Mo)")
    if(x == 9) return("9 (~10Ko)")
    return(paste(x, "(~200Ko)"))
  }
  
  dt$`#stream` <- sapply(dt$`#stream`, add_sizes)
  
  
  
  isp.list <- names(table(dt$isp))
  browser.list <- names(table(dt$browser))
  stream.list <- names(table(dt$`#stream`))
  
  dir.create("plots/p2p_ratio", showWarnings = FALSE)
  
  give.n <- function(x) { return(c(y = 0, label = length(x))) }
  
  for(this.isp in isp.list) {
    
    dir.create(paste("plots/p2p_ratio/isp", this.isp), showWarnings = FALSE)
    
    for(this.browser in browser.list) {
      
      dt1 <- dt[isp == this.isp & browser == this.browser, ]
      
      if(dim(dt1)[1] > 25) {
        
        p1 <- ggplot(dt1, aes(x = `#stream`, y = p2p.ratio)) + geom_boxplot(aes(group = `#stream`)) +
          theme(panel.grid.major = element_line(colour = "red", linetype = "dotted"), panel.grid.minor = element_line(colour = "red", linetype = "dotted")) +
          
          stat_summary(fun.data = give.n, geom = "text", fun.y = median, position = position_dodge(width = 0.75), size = 2.5, vjust = 2) + 
          
          ## stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=6, vjust = -0.5) +
          
          ggtitle(paste0("p2p ratio function of video for \nisp = ", this.isp, ", browser = ", this.browser, ", dataset size = ", dim(dt1)[1], "."))
        
        ggsave(paste0("plots/p2p_ratio/isp ", this.isp, "/", this.isp, "_", this.browser, ".png"), p1, dpi = 1000)
        
      }
    }
    
  }
}



study.p2p.ratio.v1 <- function(dataset)
{
  dt <- dataset[!is.na(cdn) & connected == "true", ]
  dt[, p2p.ratio := (p2p / (cdn + p2p))]
  dt$`#stream` <- as.character(dt$`#stream`)
  
  isp.list <- names(table(dt$isp))
  browser.list <- names(table(dt$browser))
  stream.list <- names(table(dt$`#stream`))
  
  give.n <- function(x) { return(c(y = 0, label = length(x))) }
  
  dir.create("plots/p2p_ratio", showWarnings = FALSE)
  
  stream.to.plot <- c("1", "4", "8", "9")
  stream.to.plot <- stream.list
  
  for(stream in stream.to.plot) {
    
    dir.create(paste("plots/p2p_ratio/stream", stream), showWarnings = FALSE)
    
    for(x in isp.list) {
      dt1 <- dt[isp == x & `#stream` == stream, ]
      
      p1 <- ggplot(dt1, aes(x = browser, y = p2p.ratio)) + geom_boxplot(aes(group = browser)) +
        theme(panel.grid.major = element_line(colour = "red", linetype = "dotted"), panel.grid.minor = element_line(colour = "red", linetype = "dotted")) +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, position = position_dodge(width = 0.75), size = 2.5, vjust = 2) + 
        ggtitle(paste0("p2p ratio function of browser for isp = ", x, " for Stream #", stream, "."))
      
      ggsave(paste0("plots/p2p_ratio/stream ", stream, "/isp_", x, ".png"), p1, dpi = 1000)
    }
  }
}



study.p2p.ratio.v2 <- function(dataset)
{
  dt <- dataset[!is.na(cdn) & connected == "true" & `#stream` != 9, ]
  dt[ , p2p.ratio := (p2p / (cdn + p2p))]
  
  add_sizes <- function(x) {
    if(x == 3) return("Type 3 (~200Ko)")
    if(x == 4 | x == 8) return("Type 2 (~100Mo)")
    return("Type 1 (~200Ko)")
  }
  
  dt$`#stream` <- sapply(dt$`#stream`, add_sizes)
  
  dt <- dt[, .(Views = .N, p2p.ratio.mean = mean(p2p.ratio), p2p.ratio.sigma = sd(p2p.ratio)), by = list(`#stream`, isp, browser)]
  
  isp.list <- names(table(dt$isp))
  browser.list <- names(table(dt$browser))
  stream.list <- names(table(dt$`#stream`))
  
  dir.create("plots/compatibility", showWarnings = FALSE)
  
  give.n <- function(x) { return(c(y = 0, label = length(x))) }
  
  for(stream in stream.list) {
    
    dir.name <- paste0("plots/compatibility/", substr(stream, 1, 6))
    dir.create(dir.name, showWarnings = FALSE)
    
    dt1 <- dt[`#stream` == stream, ]
    
    p01 <- ggplot(dt1, aes(x = browser, y = isp, label = round(p2p.ratio.mean, 3))) + geom_raster(aes(fill = p2p.ratio.mean)) + geom_text(colour = "red") +
      geom_text(aes(x = browser, y = isp, label = Views), vjust = 3, colour = "green") +
      ggtitle(paste0("Mean of the p2p.ratio (red) and number of views (green)\nfunction of browser and isp for videos of ", stream, "."))
    
    p02 <- ggplot(dt1, aes(x = browser, y = isp, label = round(p2p.ratio.sigma, 3))) + geom_raster(aes(fill = p2p.ratio.sigma, label = Views)) + geom_text(colour = "red") +
      geom_text(aes(x = browser, y = isp, label = Views), vjust = 3, colour = "green") +
      ggtitle(paste0("Standard deviation of the p2p.ratio (red) and number of views (green)\nfunction of browser and isp for videos of ", stream, "."))
      
    ggsave(paste0(dir.name, "/p2p_ratio_mean.png"), p01, dpi = 1000)
    ggsave(paste0(dir.name, "/p2p_ratio_stdev.png"), p02, dpi = 1000)
  }
}



study.dataset(dataset)
study.connectivity(dataset)
study.p2p.ratio.v0(dataset)
study.p2p.ratio.v1(dataset)
study.p2p.ratio.v2(dataset)



