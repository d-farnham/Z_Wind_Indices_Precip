
HEIDKE_SS = function(pred, lower, upper, obs){
	cat0 = data.frame(matrix(nrow = length(obs), ncol = 2))
	colnames(cat0) = c("pred", "obs")
	cat0$pred = ifelse(pred > lower & pred < upper, 2, ifelse(pred > lower, 3, 1))
	cat0$obs = ifelse(obs > lower & obs < upper, 2, ifelse(obs > lower, 3, 1))

	Num_Cor = sum(cat0$pred == cat0$obs)
	Tot = length(cat0$obs)
	Exp_Cor = length(cat0$obs)/3

	heidke_ss = (Num_Cor-Exp_Cor)/(Tot-Exp_Cor)
	return(heidke_ss)
	
}

MAE_SS = function(pred, clim, obs, std.dev){
	MAE_clim = sum(abs((obs-clim)/std.dev))
	MAE_pred = sum(abs((obs-pred)/std.dev))
	
	mae_ss = 1 - (MAE_pred/MAE_clim)
	
	return(mae_ss)
	
}
