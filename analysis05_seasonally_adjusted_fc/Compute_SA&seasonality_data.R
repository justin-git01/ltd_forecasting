###########################################################################

# This script performs a decomposition for all variables including all ltd,
# and exploratory variables (sales, hvi, lending).
# Then filter out seasonally adjusted data and seasonality

###########################################################################

# Load ltd aggregate date
ltd_agg <- read_excel("data/LTD_new.xlsx", sheet = 1) |>
  rename(Date = ...1,
         ltd = LTD,
         sales = SALES,
         hvi = HVI,
         lending = LENDING) |>
  dplyr::select(c(Date, ltd, sales, hvi, lending))

# Load ltd unit data and join with aggregate data
ltd_unit <- read_excel("data/LTD_new.xlsx", sheet = 2) |>
  rename(Date = ...1) |>
  dplyr::select(Date, ltd_total, ltd_nonres, ltd_comm, ltd_ind, ltd_other, ltd_res) |>
  left_join(ltd_agg, by = c("Date")) |>
  dplyr::select(-ltd)

# Rename ltd unit data
names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi", "lending")

# Convert ltd unit data to tsibble object
ltd_unit <- ltd_unit %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)

# Filter out lending as it does not require seasonally adjusted
lend <- ltd_unit |>
  select(lending) |>
  relocate(Month)

# Total
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(Total))

## Filter out SA
total_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Total = season_adjust)

## Filter out seasonality
total_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(Total = season_year)

# Residential
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(Res))

## Filter out SA
res_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Res = season_adjust)

## Filter out seasonality
res_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(Res = season_year)

# NonRes
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(NonRes))

## Filter out SA
nonres_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(NonRes = season_adjust)

## Filter out seasonality
nonres_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(NonRes = season_year)

# Comm
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(Comm))

## Filter out SA
comm_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Comm = season_adjust)

## Filter out seasonality
comm_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(Comm = season_year)

# Ind
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(Ind))

## Filter out SA
ind_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Ind = season_adjust)

## Filter out seasonality
ind_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(Ind = season_year)

# other
## Decomp
dcmp <- ltd_unit |> 
  model(stl = STL(Other))

## Filter out SA
other_ltd_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Other = season_adjust)

## Filter out seasonality
other_ltd_s <- components(dcmp) |>
  select(season_year) |>
  relocate(Month) |>
  rename(Other = season_year)

# Sales
# Decompositions
dcmp <- ltd_unit |>
  model(stl = STL(Sales))

# Filter out sales trend
sales_trend <- components(dcmp) |>
  select(trend) |>
  relocate(Month) |>
  rename(sales_trend = trend)

# Filter out SA sales
sales_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(Sales = season_adjust)

# HVI
# Decompositions
dcmp <- ltd_unit |>
  model(stl = STL(hvi))

# Filter out SA hvi
hvi_sa <- components(dcmp) |>
  select(season_adjust) |>
  relocate(Month) |>
  rename(hvi = season_adjust)


# Seasonal adjusted data
sa_data <- total_ltd_sa |>
  left_join(nonres_ltd_sa, by = c("Month")) |>
  left_join(comm_ltd_sa, by = "Month") |>
  left_join(ind_ltd_sa, by = "Month") |>
  left_join(other_ltd_sa, by = "Month") |>
  left_join(res_ltd_sa, by = "Month") |>
  left_join(sales_sa, by = "Month") |>
  left_join(hvi_sa, by = "Month") |>
  left_join(lend, by = "Month") |>
  left_join(sales_trend, by = "Month")

# Seasonal data
s_data <- total_ltd_s |>
  left_join(nonres_ltd_s, by = c("Month")) |>
  left_join(comm_ltd_s, by = "Month") |>
  left_join(ind_ltd_s, by = "Month") |>
  left_join(other_ltd_s, by = "Month") |>
  left_join(res_ltd_s, by = "Month") 


# Train-test set for SA data
sa_data <- sa_data[1:117,]
save(sa_data, file = "data/seasonally_adjusted_dat.RData")

# Train-test set for Seasonal data
seasonal_data <- s_data[1:117,]
save(seasonal_data, file = "data/seasonal_dat.RData")


