


# 1
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
Rdm_models_1 <- Rdm_models

# 2
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_2\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Jan_2024_03_05_13.RData")
Rdm_models_2 <- Rdm_models

# 3
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")


Rdm_models <- c(Rdm_models_1, Rdm_models_2, Rdm_models)
# Rdm_models <- c(Rdm_models_1, Rdm_models, Rdm_models_2)
rm(Rdm_models_1, Rdm_models_2)

length(Rdm_models)

save(Rdm_models, SG_Variables_Selected, roundingDelta, file = "Sable_Combo_2022_FCNN_model_60_Rdm_models.RData")


# Copy and paste the script to run
Predict_NN_Age_Script.R



# Below uses 'Rdm_models' (above) and 'newScans' saved from running Predict_NN_Age_Script.R

Delta
# Delta <- -0.05
Correlation_R_squared_RMSE_MAE_SAD_Table <- NULL
Correlation_R_squared_RMSE_MAE_SAD_Table_rd <- NULL  # rd = round


# verbose = 0 in keras:::predict.keras.engine.training.Model
for(N in c(1, 10, 20, 30, 40, 50, 60)) {  

    newScans.pred.ALL <- NULL
        for(j in 1:N) {
           Fold_models <- Rdm_models[[j]]
           for (i in 1:length(Fold_models)) {      
               newScans.pred <- as.vector(keras:::predict.keras.engine.training.Model(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans), verbose = 0))
               newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
        }
    }  
	
	print(dim(newScans.pred.ALL))
	print(nrow(newScans.pred.ALL)/length(unique(newScans.pred.ALL$Index)))
      
    Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median, na.rm = TRUE)[,2], 
      Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025, na.rm = TRUE)[,2],
      Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975, na.rm = TRUE)[,2]), 4)
    
    cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", N, " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))    
    
	
	# New_Ages <- data.frame(filenames = Model_Spectra_Meta$filenames, Pred_median) # Model_Spectra_Meta available from running 'Predict_NN_Age_Script.R' above
    # New_Ages$TMA <- NULL # Clear old TMA before updating
    # if(length(get.subs(get.subs(New_Ages$filenames[1], sep = "."))) == 2)
    #    New_Ages$filenames <- get.subs(New_Ages$filenames, sep = ".")[1,]
    # New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'filenames', 'filenames', 'TMA')  
	
	New_Ages_Temp <- data.frame(filenames = Model_Spectra_Meta$filenames, Pred_median, TMA = Model_Spectra_Meta$TMA)
    
    cat("\n\nPrediction not rounded:\n")       
	print(Correlation_R_squared_RMSE_MAE_SAD_Table <- rbind(Correlation_R_squared_RMSE_MAE_SAD_Table, 
	                data.frame(N = N, t(as.matrix(Correlation_R_squared_RMSE_MAE_SAD(New_Ages_Temp$TMA, New_Ages_Temp$NN_Pred_Median))))))	
					
	cat(paste0("\nPrediction rounded after adding a Delta of: ", Delta, "\n")) 	   
    print(Correlation_R_squared_RMSE_MAE_SAD_Table_rd <- rbind(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, 
	                data.frame(N = N, t(as.matrix(Correlation_R_squared_RMSE_MAE_SAD(New_Ages_Temp$TMA, round(New_Ages_Temp$NN_Pred_Median + Delta)))))))
    }    
	
library(JRWToolBox)
	

browsePlot('
    plot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd$N, Correlation_R_squared_RMSE_MAE_SAD_Table_rd$SAD, xlab = "Number of Random Replicates of Full Fold Models", ylab = "SAD", 
                col = "green", ylim = c(1900, 2100), type = "o")
     				
	points(Correlation_R_squared_RMSE_MAE_SAD_Table$N, Correlation_R_squared_RMSE_MAE_SAD_Table$SAD, col = "dodgerblue", type = "o")		
')				
		

# Rounded prediction is green, not rounded is Dodger blue
browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "SAD", xlab = "Number of Random Replicates of Full Fold Models", ylab = "SAD", col = "green", ylim = c(1900, 2100), lineType = "b")
	points(Correlation_R_squared_RMSE_MAE_SAD_Table$N, Correlation_R_squared_RMSE_MAE_SAD_Table$SAD, col = "dodgerblue", type = "b")		
')				
	


# With zero on the y-axis
browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "SAD", xlab = "Number of Random Replicates of Full Fold Models", ylab = "SAD", col = "green", ylim = c(-5, 2400), lineType = "b")
	points(Correlation_R_squared_RMSE_MAE_SAD_Table$N, Correlation_R_squared_RMSE_MAE_SAD_Table$SAD, col = "dodgerblue", type = "b")		
')						
		


browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "RMSE", xlab = "Number of Random Replicates of Full Fold Models", ylab = "RMSE", col = "green", ylim = c(2, 2.5), lineType = "b")
	points(Correlation_R_squared_RMSE_MAE_SAD_Table$N, Correlation_R_squared_RMSE_MAE_SAD_Table$RMSE, col = "dodgerblue", type = "b")		
')				
			
	

browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "RMSE", xlab = "Number of Random Replicates of Full Fold Models", ylab = "RMSE", col = "green", ylim = c(-0.5, 2.5), lineType = "b")
	points(Correlation_R_squared_RMSE_MAE_SAD_Table$N, Correlation_R_squared_RMSE_MAE_SAD_Table$RMSE, col = "dodgerblue", type = "b")		
')				
			
			
	
# --- Order (1,2,3) vs (1,3,2) for rounded predictions ---

# Order (1,2,3) is green, order (1,3,2) is Dodger blue
browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "SAD", xlab = "Number of Random Replicates of Full Fold Models", ylab = "SAD", col = "green", ylim = c(1880, 1980), lineType = "b", pch = 4)
	points(Correlation_R_squared_RMSE_MAE_SAD_Table_rd_1_3$N, Correlation_R_squared_RMSE_MAE_SAD_Table_rd_1_3$SAD, col = "dodgerblue", type = "b")		
', file = "Sable SAD vs Num of Random Replicates of Full Fold Modesl.png")				
		


# With zero on the y-axis
browsePlot('
    gPlot(Correlation_R_squared_RMSE_MAE_SAD_Table_rd, "N", "SAD", xlab = "Number of Random Replicates of Full Fold Models", ylab = "SAD", col = "green", ylim = c(-5, 2400), lineType = "b", pch = 4)
	points(Correlation_R_squared_RMSE_MAE_SAD_Table_rd_1_3$N, Correlation_R_squared_RMSE_MAE_SAD_Table_rd_1_3$SAD, col = "dodgerblue", type = "b")		
	abline(h=1903, lty = 2)
', file = "Sable SAD vs Num of Random Replicates of Full Fold Modesl with Zero on Y axis.png")				
	












