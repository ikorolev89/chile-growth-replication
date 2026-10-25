# Data sources and construction

The analysis uses the frozen inputs supplied by the authors. Replacing them with a current download can change historical observations and the donor pool.

| Input | Source and role |
|---|---|
| `data/raw/gdp_pc.xlsx` | Author-supplied extract of World Bank real GDP per capita, indicator NY.GDP.PCAP.KD (constant 2015 US dollars). |
| `data/raw/maddison.xlsx` | Author-supplied extract attributed in the paper to the Maddison Project Database 2023, covering years through 2022. |
| `data/chile_data_wb.csv` | Long panel prepared from the World Bank workbook, 1960–2014; units with any missing observation in that window excluded. Natural logarithm added. |
| `data/chile_data_mad.csv` | Long panel prepared from the Maddison workbook, 1950–2014; units with any missing observation in that window excluded. Estimation begins in 1951. |
| `data/country_codes_*.csv` | Author-supplied donor-pool definitions. The numeric suffixes are historical file labels, not guaranteed counts of usable donors. |

These workbooks are author-prepared extracts, not asserted to be unmodified vendor downloads. The exact original download dates and the earlier workbook-extraction steps were not recorded in the supplied files. SHA-256 hashes of all distributed inputs appear in `data_manifest.csv`.

Re-preparing the workbooks yields all 4,895 World Bank observations and all 9,425 Maddison observations in the saved CSVs. The maximum log difference is below 5e-14. The original saved wide R objects also agree exactly with the CSVs; the portable workflow reconstructs them instead of distributing saved author sessions.

The usable donor counts **excluding Chile** are 16, 24, and 42 for World Bank, and 17, 25, and 43 for Maddison. The largest sample has 43/44 units **including Chile**. Details and excluded country codes are in `verification/donor_pools.csv`.

Source links and attribution:

- [World Bank indicator NY.GDP.PCAP.KD](https://data.worldbank.org/indicator/NY.GDP.PCAP.KD), labeled CC BY 4.0; [World Bank data licensing terms](https://datacatalog.worldbank.org/public-licenses). Transformations here: author extraction, reshaping, sample selection, and natural logarithms.
- [Maddison Project Database 2023](https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023), [data DOI](https://doi.org/10.34894/INZBF2), licensed CC BY 4.0. Cite Jutta Bolt and Jan Luiten van Zanden, “Maddison-style estimates of the evolution of the world economy: A new 2023 update,” *Journal of Economic Surveys* 39 (2025), 631–671, [article DOI](https://doi.org/10.1111/joes.12618). The release page additionally requires citation of original source papers for graphical displays or subsets of fewer than 12 countries; the complete source lists and citation notes are preserved in `data/source_notes/` for attribution when reusing these data.

The data retain their source terms; a software license for the authors' code would not replace them.

## Maddison underlying sources

The database's complete bibliographic source lists are included as UTF-8 CSVs:

- `data/source_notes/maddison_2023_sources.csv`: the **Sources** sheet.
- `data/source_notes/maddison_original_sources.csv`: the **Maddison original sources** sheet.
- `data/source_notes/maddison_2023_notes.csv`: the **Notes** sheet, including the citation policy.

The cells retain their original order and text; empty cells are blank and generic column headings have been added. Country headings apply to the following continuation rows. These files preserve attribution for Chile and the donor countries. In particular, the source sheet attributes Chile's 1810–2004 series to Díaz, J. B., Lüders, R., and Wagner, G. (2007), *Economia Chilena 1810–2000, Producto total y sectorial una nueva mirada*, Pontificia Universidad Católica de Chile, Instituto de Economía, Documento de Trabajo no. 315. The sheet's general update note attributes GDP per capita for 2008–2022 to the Conference Board Total Economy Database, except where indicated. See the complete source lists for country-specific qualifications.

These bibliographic sheets were extracted on October 25, 2026 from the [official MPD 2023 workbook](https://dataverse.nl/api/access/datafile/421302), `mpd2023_web.xlsx`. This download supplies documentation only: the frozen analysis inputs have not been replaced or updated. The downloaded workbook's SHA-256 is `ecc5916ca12789b983fc4be437f8a354bbf4291323605324ac3e0aea4c57cbb6`. The extracted documentation retains the database's [CC BY 4.0 license](https://creativecommons.org/licenses/by/4.0/).
