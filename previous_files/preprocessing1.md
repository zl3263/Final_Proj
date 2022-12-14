processing
================
2022-11-25

# Preprocessing

## parts to be completed (add as your wish)

| Contents                          | Stage | Comments                |
|-----------------------------------|-------|-------------------------|
| separate observations             | √     |                         |
| NAs (error value)                 |       | na.omit()  other error? |
| add explanations to each variable |       |                         |
| link the address to lat&lon       |       | other ways to plot map? |
| simple scatter/box plots          |       |                         |

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(sf)
```

    ## Linking to GEOS 3.10.2, GDAL 3.4.2, PROJ 8.2.1; sf_use_s2() is TRUE

In the origin data, each properties are compared with three comparable
rentals. So in total, information of four building(sets) forms one
observation.

``` r
comparable_rental_income_raw = read_csv("data/DOF__Cooperative_Comparable_Rental_Income__Citywide.csv") %>%
  janitor::clean_names()
```

    ## Rows: 44960 Columns: 60
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): Boro-Block-Lot, Address, Neighborhood, Building Classification, Bo...
    ## dbl (44): Total Units, Year Built, Gross SqFt, Estimated Gross Income, Gross...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(comparable_rental_income_raw)
```

    ## # A tibble: 6 × 60
    ##   boro_block_lot address neigh…¹ build…² total…³ year_…⁴ gross…⁵ estim…⁶ gross…⁷
    ##   <chr>          <chr>   <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 1-00011-0014   26 BEA… FINANC… D4  -E…      21    1909   51387  2.40e6    46.7
    ## 2 1-00028-0001   3 HANO… FINANC… D0  -E…     211    1926  239121  9.46e6    39.6
    ## 3 1-00094-0001   80 GOL… SOUTHB… D4  -E…    1652    1971 2155492  6.81e7    31.6
    ## 4 1-00100-0026   138 NA… SOUTHB… D4  -E…      43    1901   79422  3.13e6    39.4
    ## 5 1-00117-0001   185 PA… CIVIC … D4  -E…     465    1960  515425  2.59e7    50.2
    ## 6 1-00134-0025   258 BR… CIVIC … D4  -E…      45    1915   88320  4.24e6    48  
    ## # … with 51 more variables: estimated_expense <dbl>, expense_per_sq_ft <dbl>,
    ## #   net_operating_income <dbl>, full_market_value <dbl>,
    ## #   market_value_per_sq_ft <dbl>, boro_block_lot_1 <chr>, address_1 <chr>,
    ## #   neighborhood_1 <chr>, building_classification_1 <chr>, total_units_1 <dbl>,
    ## #   year_built_1 <dbl>, gross_sq_ft_1 <dbl>, estimated_gross_income_1 <dbl>,
    ## #   gross_income_per_sq_ft_1 <dbl>, estimated_expense_1 <dbl>,
    ## #   expense_per_sq_ft_1 <dbl>, net_operating_income_1 <dbl>, …

### Seperate the observations:

so I would like to separate one and 3 comparables with into four subset
with same variable names for further processing.

delete `distance_from_co_op_in_miles*` to make the variables consistent
to each other.

``` r
namelist = comparable_rental_income_raw %>%
  select(-c("distance_from_co_op_in_miles","distance_from_co_op_in_miles_1","distance_from_co_op_in_miles_2")) %>%
  colnames()

## to be optimized 

rentalincom_ori = comparable_rental_income_raw %>%
  select(append(namelist[1:14],"report_year"))

std_colname = colnames(rentalincom_ori)

rentalincom_c1 = comparable_rental_income_raw %>%
  select(append(namelist[15:28],"report_year"))
colnames(rentalincom_c1) = std_colname

rentalincom_c2 = comparable_rental_income_raw %>%
  select(append(namelist[29:42],"report_year"))
colnames(rentalincom_c2) = std_colname
  
rentalincom_c3 = comparable_rental_income_raw %>%
  select(append(namelist[43:56],"report_year"))
colnames(rentalincom_c3) = std_colname

transformed_rental_income = rbind(rentalincom_ori,rentalincom_c1,rentalincom_c2,rentalincom_c3)
```

### clean data

``` r
transformed_rental_income = transformed_rental_income %>% 
  na.omit() 
```

### add zoning info for the dataset

Because our data only include `boro_block_lot`, which is the tax
identification of a building, if we need to mark the building on the
map, we need to map the zoning of the building on the map.

``` r
taxlot_to_zoning = read_csv("data/taxlot_to_zoning.csv") %>%
  janitor::clean_names() %>%
  mutate(
    boro_block = as.numeric(borough_code * 100000 + tax_block)
  ) %>%
  select(boro_block,zoning_district_1) %>%
  distinct(boro_block, .keep_all = TRUE)
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 858266 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): Zoning District 1, Zoning District 2, Commercial Overlay 1, Special...
    ## dbl (4): Borough Code, Tax Block, Tax Lot, BBL
    ## lgl (5): Zoning District 3, Zoning District 4, Commercial Overlay 2, Special...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
expanded_df = 
  tibble(boro_block = 1:600000) %>%
  full_join(taxlot_to_zoning,by = "boro_block")

transformed_rental_income =
  transformed_rental_income %>%
  mutate(
    block_id = as.numeric(substr(boro_block_lot,3,7)),
    boro_block = as.numeric(paste0(substr(boro_block_lot,1,1),substr(boro_block_lot,3,7))),
    zoning = expanded_df$zoning_district_1[boro_block]
  )
```

### Data description

Boro-Block-Lot: Borough-Block-Lot(BBL) location

``` r
comparable_rental_income_raw %>% distinct(building_classification)
```

    ## # A tibble: 12 × 1
    ##    building_classification
    ##    <chr>                  
    ##  1 D4  -ELEVATOR          
    ##  2 D0  -ELEVATOR          
    ##  3 C6  -WALK-UP           
    ##  4 C8  -WALK-UP           
    ##  5 D4-ELEVATOR            
    ##  6 C6-WALK-UP             
    ##  7 D4  -ELEVATOR          
    ##  8 D0  -ELEVATOR          
    ##  9 C6  -WALK-UP           
    ## 10 C8  -WALK-UP           
    ## 11 D0-ELEVATOR            
    ## 12 C8-WALK-UP

``` r
comparable_rental_income_raw %>% distinct(neighborhood)
```

    ## # A tibble: 295 × 1
    ##    neighborhood   
    ##    <chr>          
    ##  1 FINANCIAL      
    ##  2 SOUTHBRIDGE    
    ##  3 CIVIC CENTER   
    ##  4 TRIBECA        
    ##  5 CHINATOWN      
    ##  6 SOHO           
    ##  7 LOWER EAST SIDE
    ##  8 ALPHABET CITY  
    ##  9 EAST VILLAGE   
    ## 10 LITTLE ITALY   
    ## # … with 285 more rows

``` r
comparable_rental_income_raw %>% distinct(year_built)
```

    ## # A tibble: 123 × 1
    ##    year_built
    ##         <dbl>
    ##  1       1909
    ##  2       1926
    ##  3       1971
    ##  4       1901
    ##  5       1960
    ##  6       1915
    ##  7       1920
    ##  8       1900
    ##  9       1925
    ## 10       1931
    ## # … with 113 more rows

Gross SqFt: Gross square footage of the building Estimated Gross Income:
Estimated Income per SquareFoot \* Gross SquareFoot Gross Income per
SqFt: Estimated income per squarefoot of median comparable Estimated
Expense: Estimated Expense per SquareFoot \* Gross SquareFoot Expense
per SqFt: Estimated expense per squarefoot of median comparab Net
Operating Income: Estimated Gross Income-Estimated Expense Full Market
Value: Current year’s total market value of the land and building Market
Value per SqFt: Full Market Value/ Gross SquareFoot Distance from Co-op
in miles: calculated distance from comparable to the subject

### Somehow, we need the location of each block.

Without copying file from `Shiny_Map`, directly use it…

``` r
ave_location = function(geom,id){
  mean(geom[[1]][[1]][,id])
}

block_edge = read_sf("Shiny_Map/DTM_Tax_Block_Polygon.shp") %>%
  st_transform("NAD83") %>%
  mutate(
    ave_long = map_dbl(geometry,~ave_location(.x,1)),
    ave_lat = map_dbl(geometry,~ave_location(.x,2)),
    boro_block = as.numeric(BORO) * 100000 + as.numeric(BLOCK)
  ) %>%
  as_tibble() %>%
  select(boro_block,ave_long,ave_lat) %>%
  arrange(boro_block) %>%
  na.omit() %>%
  group_by(boro_block) %>%
  summarise(
    long = mean(ave_long),
    lat = mean(ave_lat)
  )
  
expanded_df = 
  tibble(boro_block = 1:600000) %>%
  full_join(block_edge)
```

    ## Joining, by = "boro_block"

``` r
transformed_rental_income =
  transformed_rental_income %>%
  mutate(
    longitude = expanded_df$long[boro_block],
    latitude = expanded_df$lat[boro_block]
  ) %>%
  select(boro_block_lot,boro_block,block_id,longitude,latitude,zoning,everything())


save(transformed_rental_income, file = "data/cleaned_data.RData")
# because shiny can only access its own directory....
save(transformed_rental_income, file = "Shiny_Map/cleaned_data.RData")
```
