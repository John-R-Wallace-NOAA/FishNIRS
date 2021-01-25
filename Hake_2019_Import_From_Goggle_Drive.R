
sourceFunctionURL <- function (URL) {
       " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
       require(httr)
       File.ASCII <- tempfile()
       on.exit(file.remove(File.ASCII))
       getTMP <- httr::GET(URL)
       write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
       source(File.ASCII, local = parent.env(environment()))
}

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")   
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/scanIn.R")
             
lib(googledrive)


# The googledrive::drive_download() only needs the 'ID' from the URLs.
# So, e.g. just "gTeg81man8g2iZHnSvfxUtWxArPfTbX7" from the URL:

#  https://drive.google.com/file/d/1gTeg81man8g2iZHnSvfxUtWxArPfTbX7/view?usp=drive_web

# googledrive::as_id() will extract the ID for you, but I copied the email source file and extracted the IDs myself.
# Those ID's are in the Hake_2019_Ots_GD_IDs_22_Jan_2021.R file on the FishNIRS repo.


# Download Hake_2019_GD_File_ID's from 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/Hake_2019_Ots_GD_IDs_22_Jan_2021.R")


temp <- tempfile(fileext = ".bin")
ldf <- list() # creates an empty list
Spc_df <- NULL

for (k in (1:length(Hake_2019_GD_File_IDs))) {

   cat("\n\n", k, "\n")
   dl <- googledrive::drive_download(googledrive::as_id(Hake_2019_GD_File_IDs[k]), path = temp,  overwrite = TRUE)
   ldf[[k]] <- simplerspec::read_opus_bin_univ(temp, extract = "spc", print_progress = TRUE, atm_comp_minus4offset = FALSE)    
   ldf[[k]][['spc']] <- data.frame(ldf[[k]][['spc']])
   Spc_df <- rbind(Spc_df, ldf[[k]][['spc']])
}


# print(str(ldf[[1]])) # check first element
cat("\n\n"); print(names(ldf[[3]]))
# cat("\n\n"); print(str(ldf[[3]]))

ldf[[1]][['spc']][, 1:20]
Spc_df[1:3, 1:5]

range(as.numeric(substring(names(ldf[[k]][['spc']]), 2)))
table(diff(as.numeric(substring(names(ldf[[k]][['spc']]), 2))))


dev.new(width = 400, height = 300)
matplot(as.numeric(substring(names(ldf[[k]][['spc']]), 2)), t(Spc_df), col = 'green', type = 'l', lty = 1,
               xlab = "Wavelength Energy (1/cm)", ylab = "Absorbance")

                
                
                
                
                
                
                
                