# Independently reconstruct the analysis CSVs from the supplied workbooks.
source("scripts/load_environment.R")
suppressPackageStartupMessages({library(readxl);library(dplyr);library(tidyr)})
for(ds in c("wb","mad")) {
  file<-if(ds=="wb")"gdp_pc.xlsx" else "maddison.xlsx"
  raw<-read_excel(file.path("data/raw",file)) |>
    pivot_longer(matches("^[0-9]{4}$"),names_to="year",values_to="GDP_pc") |>
    mutate(year=as.integer(year),GDP_pc=as.numeric(GDP_pc)) |>
    filter(year>=if(ds=="wb")1960 else 1950,year<=2014) |>
    group_by(CountryName) |> filter(!any(is.na(GDP_pc))) |> ungroup() |>
    mutate(lnGDP_pc=log(GDP_pc)) |>
    select(CountryName,CountryCode,year,GDP_pc,lnGDP_pc) |> arrange(CountryCode,year)
  csv<-read.csv(file.path("data",paste0("chile_data_",ds,".csv"))) |>
    select(CountryName,CountryCode,year,GDP_pc,lnGDP_pc) |> arrange(CountryCode,year)
  stopifnot(identical(as.data.frame(raw[1:3]),as.data.frame(csv[1:3])),
            max(abs(raw$GDP_pc-csv$GDP_pc))<1e-8,
            max(abs(raw$lnGDP_pc-csv$lnGDP_pc))<1e-12)
  cat(ds,":",nrow(csv),"observations reproduce.\n")
}
