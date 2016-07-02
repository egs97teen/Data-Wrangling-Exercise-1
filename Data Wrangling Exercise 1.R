# load original file
original <- read.csv("C:/Users/EGGS/Desktop/refine_original.csv")

# view original file
View(original)

#load libraries
library("dplyr")
library("tidyr")

# organize brand names
refined <- tbl_df(original)
refined$company[1:6] <- "philips"
refined$company[7:13] <- "akzo"
refined$company[14:16] <- "philips"
refined$company[17:21] <- "van houten"
refined$company[22:25] <- "unilever"

# separate product codes and numbers

refined_final <- refined %>% 
  rename(ProductCode = Product.code...number) %>% 
  separate(ProductCode, c("product_code","product_number")) %>%
  
  # add categories
  mutate(product_category=ifelse(product_code=="p","Smartphone",
                                ifelse(product_code=="v","TV",
                                        ifelse(product_code=="x","Laptop",
                                               ifelse(product_code=="q","Tablet","NA"))))) %>%
  
  # add address
  unite("Address",address,city,country, sep=",",remove=FALSE) %>%
  arrange(company,product_code,name) %>%
  
  # create dummy variables for company
  mutate(company_philips=ifelse(company=="philips",1,0)) %>%
  mutate(company_akzo=ifelse(company=="akzo",1,0)) %>%
  mutate(company_van_houten=ifelse(company=="van houten",1,0)) %>%
  mutate(company_unilever=ifelse(company=="unilever",1,0)) %>%
  
  #create dummy variables for products
  mutate(product_smartphone=ifelse(product_category=="Smartphone",1,0)) %>%
  mutate(product_tv=ifelse(product_category=="TV",1,0)) %>%
  mutate(product_laptop=ifelse(product_category=="Laptop",1,0)) %>%
  mutate(product_tablet=ifelse(product_category=="Tablet",1,0))

# view final
View(refined_final)

# write final
write.csv(refined_final, file = "refine_clean.csv")
  
  