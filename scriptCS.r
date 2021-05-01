library(gridExtra)

model1_AIC <- model1$AIC
model1_BIC <- model1$BIC


theft_results <- data_frame(Model = "Model 1", 
                            AIC = model1_AIC,
                            BIC = model1_BIC)

theft_results %>% knitr::kable(caption = "Test Title") %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:3, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

model2_AIC <- 700
model2_BIC <- 600

theft_results <- bind_rows(theft_results,
  data_frame(Model = "Model 2", 
             AIC = model2_AIC,
             BIC = model2_BIC))

theft_results %>% knitr::kable(caption = "Test Title") %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:3, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

model3_AIC <- 500
model3_BIC <- 500

theft_results <- bind_rows(theft_results,
                           data_frame(Model = "Model 3", 
                                      AIC = model3_AIC,
                                      BIC = model3_BIC))

theft_results %>% knitr::kable(caption = "Test Title") %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:3, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")


theft_ts %>% ggplot()

x <- seq(1,10,1)
y <- seq(1,10,1)

df_test2 <- data.frame(x = x, y = y)

p1 <- df_test1 %>% ggplot(aes(x = x, y = y)) +
  geom_line()



p2 <- df_test2 %>% ggplot(aes(x = x, y = y)) +
  geom_line()


grid.arrange(p1, p2)


