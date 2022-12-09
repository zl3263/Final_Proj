library(tidyverse)
library(rgl)

load("data/cleaned_data.RData")

n_neighbours = 20

transformed_rental_income = 
  transformed_rental_income %>%
  mutate(
    is_elevator = str_detect(building_classification,"ELEVATOR")
  )

linear_model = lm(gross_income_per_sq_ft~ is_elevator+total_units+year_built+report_year+gross_sq_ft, data=transformed_rental_income)

transformed_rental_income = 
  transformed_rental_income %>%
  mutate(
    resid_linear = linear_model$residuals
  )  %>%
  group_by(address) %>%
  summarize(
    gross_income_per_sq_ft = mean(gross_income_per_sq_ft),
    resid_linear = mean(resid_linear),
    longitude = mean(longitude),
    latitude = mean(latitude),
    is_elevator = as.logical(mean(is_elevator)),
    total_units = mean(total_units),
    year_built = mean(year_built),
    report_year = mean(report_year),
    gross_sq_ft = mean(gross_sq_ft)
  ) 


lowess_surf = loess(resid_linear~latitude+longitude,data=transformed_rental_income,span = 0.05)

transformed_rental_income=
  transformed_rental_income %>%
  mutate(
  resid_smooth = lowess_surf$fitted
)  

transformed_rental_income %>%
  ggplot(aes(x=longitude,y=latitude,color=resid_smooth)) +
  geom_point()

smooth_location_resid = transformed_rental_income %>%
  select(longitude,latitude,resid_smooth)

predict_rental = function(new_data){
  if(nrow(new_data)!=1){
    return("nrow must be 1")
  }
  location_resid =
    smooth_location_resid %>%
    mutate(
      d=(longitude-new_data$longitude)^2+(latitude-new_data$latitude)^2
    ) %>%
    arrange(d) %>% 
    head(n_neighbours) %>%
    lm(resid_smooth~longitude+latitude,data=.) %>%
    predict(newdata = new_data)
  
  predict(linear_model,newdata=new_data) + location_resid
}



model_coef =
  broom::tidy(linear_model) %>%
  select(term,estimate)

save(predict_rental,linear_model,smooth_location_resid,model_coef,file = "Index_Predictor/modeling_result.RData")



