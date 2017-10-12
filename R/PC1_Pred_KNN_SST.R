# now do the same thing for Dec
load("data/Processed data/JFM_U_preds.RData")
load("data/Processed data/climate_ind.Rdata")
load("data/Processed data/D_SST_preds.RData")

climate_ind_D = climate_ind %>% dplyr::filter(month %in% c(12)) %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::select(-month)

JFM_D_preds = merge(merge(JFM_U_preds, climate_ind_D, by = "year"), D_SST_preds, by = "year")

require(corrplot)
require(psych)
M = cor(JFM_D_preds)
M_rank = cor(JFM_D_preds, method = "spearman",use = "complete.obs")

pval <- psych::corr.test(JFM_D_preds, adjust="none", method="spearman")$p

pdf('Ref figures/SST_U_PCs_cors_spearman_JFM_D.pdf', height = 12, width = 12)
corrplot(M_rank, p.mat=pval, sig.level = 0.05)
dev.off()


# scale the PC1 
JFM_D_preds = JFM_D_preds %>% dplyr::mutate(PC1_scale = (PC1 - mean(PC1))/sd(PC1),
                                            PC3_scale = (PC3 - mean(PC3))/sd(PC3),
                                            PC6_scale = (PC6 - mean(PC6))/sd(PC6),
                                            NINO_EC1 = NINO1_2 - NINO4,
                                            NINO_EC2 = NINO3 - NINO4)

# training set should include the first 50 years of data
training = JFM_D_preds %>% dplyr::filter(year < 2016)

library(kknn)


# use leave one-out CV to choose k for each of the models
par(mfrow = c(2,1))
train_mod1 = train.kknn(formula = PC1 ~ SST_PC1, data = training, kmax = 20, kernel = "gaussian")
plot(train_mod1)
train_mod2 = train.kknn(formula = PC1 ~ SST_PC1 + SST_PC3, data = training, kmax = 20, kernel = "gaussian")
plot(train_mod2)



k.mod1 = 8
k.mod2 = 8
scale.models = TRUE
kernel.models = "gaussian"
cand.model.1 = PC1 ~ SST_PC1
cand.model.2 = PC1 ~ SST_PC1 + SST_PC4 + SST_PC3 + SST_PC2


set.seed(22)
# bootstrap to get the confidence interval for the prediction
n.boots = 1000
train = data.frame(matrix(nrow = nrow(training), ncol = n.boots))
for(bboot in 1:n.boots){
  
  tmp_data = training[sample(1:nrow(training),nrow(training),replace = TRUE),]
  
  tmp_knn_out = kknn(formula = cand.model.1, train = tmp_data, test = training, k = k.mod1, scale = scale.models, kernel = kernel.models)
  train[,bboot] = tmp_knn_out$fitted.values
}

pred = data.frame(matrix(nrow = 1, ncol = n.boots))
for(bboot in 1:n.boots){
  
tmp_data = training[sample(1:nrow(training),nrow(training),replace = TRUE),]
    
tmp_knn_out = kknn(formula = cand.model.1, train = tmp_data, test = JFM_D_preds %>% dplyr::filter(year == 2016), k = k.mod1, scale = scale.models, kernel = kernel.models)
pred[1,bboot] = tmp_knn_out$fitted.values
}

train2 = data.frame(matrix(nrow = nrow(training), ncol = n.boots))
for(bboot in 1:n.boots){
  
  tmp_data = training[sample(1:nrow(training),nrow(training),replace = TRUE),]
  
  tmp_knn2_out = kknn(formula = cand.model.2, train = tmp_data, test = training, k = k.mod2, scale = scale.models, kernel = kernel.models)
  train2[,bboot] = tmp_knn2_out$fitted.values
}

pred2 = data.frame(matrix(nrow = 1, ncol = n.boots))
for(bboot in 1:n.boots){
  
  tmp_data = training[sample(1:nrow(training),nrow(training),replace = TRUE),]
  
  tmp_knn2_out = kknn(formula = cand.model.2, train = tmp_data, test = JFM_D_preds %>% dplyr::filter(year == 2016), k = k.mod2, scale = scale.models, kernel = kernel.models)
  pred2[1,bboot] = tmp_knn2_out$fitted.values
  
}


# add year column to all of these data.frames

train$year = train2$year = training$year
pred$year = pred2$year = 2016

train_long = melt(train, id.vars = "year") %>% dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(value),
                   top = quantile(value, 0.9),
                   bot  = quantile(value, 0.1))

train2_long = melt(train2, id.vars = "year") %>% dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(value),
                   top = quantile(value, 0.9),
                   bot  = quantile(value, 0.1))

pred_long = melt(pred, id.vars = "year") %>% dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(value),
                   top = quantile(value, 0.95),
                   bot  = quantile(value, 0.05))

pred2_long = melt(pred2, id.vars = "year") %>% dplyr::group_by(year) %>%
  dplyr::summarise(mean = mean(value),
                   top = quantile(value, 0.9),
                   bot  = quantile(value, 0.1))


mod_cor = merge(train_long %>% dplyr::select(year, mean),
                JFM_D_preds %>% dplyr::select(year, PC1),
                by = "year", all.x = TRUE)

mod_rank_cor = cor.test(mod_cor$mean, mod_cor$PC1, method = "spearman")
mod_lin_cor = cor.test(mod_cor$mean, mod_cor$PC1, method = "pearson")

mod_fit = 
  ggplot() +
  geom_line(data = train_long, aes(year, mean), col = "blue") +
  geom_line(data = train_long, aes(year, bot), linetype = "dashed", col = "blue") +
  geom_line(data = train_long, aes(year, top), linetype = "dashed", col = "blue") +
  geom_line(data = JFM_D_preds %>% dplyr::filter(year != 2017), aes(year, PC1)) +
  geom_point(data = JFM_D_preds %>% dplyr::filter(year %in% c(1983, 1998, 2003, 2016)), aes(year, PC1), size = 3) +
  geom_point(data = train_long %>% dplyr::filter(year %in% c(1983, 1998, 2003)), aes(year, mean), col = "blue", size = 3) +
  geom_point(data = pred_long, aes(year, mean), col = "red", size = 3) +
  geom_point(data = pred_long, aes(year, bot), col = "red", shape = 95, size = 7.5) +
  geom_point(data = pred_long, aes(year, top), col = "red", shape = 95, size = 7.5) +
  labs(x = "Year",
       y = "PC1 Index") +
  ggtitle(paste0("Mod 1 (SST PC1); k = ", k.mod1)) +
  geom_text(aes(x = 1997.5, y = 45, label = paste0("cor(train) = ", round(mod_lin_cor$estimate,2))), hjust= "right", colour = "blue") +
  geom_text(aes(x = 1997.5, y = 37.5, label = paste0("rank cor(train) = ", round(mod_rank_cor$estimate,2))), hjust= "right", colour = "blue") +
  scale_x_continuous(limits = c(1950,2017),
                     breaks = seq(1950,2015, by = 5)) +
  theme_bw()


mod2_cor = merge(train2_long %>% dplyr::select(year, mean),
                JFM_D_preds %>% dplyr::select(year, PC1),
                by = "year", all.x = TRUE)

mod2_rank_cor = cor.test(mod2_cor$mean, mod2_cor$PC1, method = "spearman")
mod2_lin_cor = cor.test(mod2_cor$mean, mod2_cor$PC1, method = "pearson")

mod2_fit = 
  ggplot() +
  geom_line(data = train2_long, aes(year, mean), col = "blue") +
  geom_line(data = train2_long, aes(year, bot), linetype = "dashed", col = "blue") +
  geom_line(data = train2_long, aes(year, top), linetype = "dashed", col = "blue") +
  geom_line(data = JFM_D_preds %>% dplyr::filter(year != 2017), aes(year, PC1)) +
  geom_point(data = JFM_D_preds %>% dplyr::filter(year %in% c(1983, 1998, 2003, 2016)), aes(year, PC1), size = 3) +
  geom_point(data = train2_long %>% dplyr::filter(year %in% c(1983, 1998, 2003)), aes(year, mean), col = "blue", size = 3) +
  geom_point(data = pred2_long, aes(year, mean), col = "red", size = 3) +
  geom_point(data = pred2_long, aes(year, bot), col = "red", shape = 95, size = 7.5) +
  geom_point(data = pred2_long, aes(year, top), col = "red", shape = 95, size = 7.5) +
  labs(x = "Year",
       y = "PC1 Index") +
  ggtitle(paste0("Mod 2 (SST PC1 + SST PC2 + SST PC3 + SST PC4); k = ", k.mod2)) +  geom_text(aes(x = 1997.5, y = 45, label = paste0("cor(train) = ", round(mod2_lin_cor$estimate,2))), hjust= "right", colour = "blue") +
  geom_text(aes(x = 1997.5, y = 37.5, label = paste0("rank cor(train) = ", round(mod2_rank_cor$estimate,2))), hjust= "right", colour = "blue") +
  scale_x_continuous(limits = c(1950,2017),
                     breaks = seq(1950,2015, by = 5)) +
  theme_bw()


pdf('Final figures/Figure_S7.pdf', height = 6, width = 9)
grid.arrange(mod_fit,
             mod2_fit,
             ncol = 1)
dev.off()


# now check the correaltion after eliminating the 1983, 1998, and 2003 years
mod_cor_subset = mod_cor %>% dplyr::filter(!(year %in% c(1983, 1998, 2003)))
mod2_cor_subset = mod2_cor %>% dplyr::filter(!(year %in% c(1983, 1998, 2003)))

mod_rank_cor = cor.test(mod_cor_subset$mean, mod_cor_subset$PC1, method = "spearman")
mod_lin_cor = cor.test(mod_cor_subset$mean, mod_cor_subset$PC1, method = "pearson")

mod2_rank_cor = cor.test(mod2_cor_subset$mean, mod2_cor_subset$PC1, method = "spearman")
mod2_lin_cor = cor.test(mod2_cor_subset$mean, mod2_cor_subset$PC1, method = "pearson")


# model 1
print(mod_lin_cor$estimate) # 0.6622427
print(mod_rank_cor$estimate) # 0.6433852

# model2
print(mod2_lin_cor$estimate) # 0.7982881
print(mod2_rank_cor$estimate) # 0.7750096



rm(list = ls())
