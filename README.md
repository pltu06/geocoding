# geocoding

Researchers should exercise diligence around geocoding private information, such as addresses. Currently, this R code utilizes the U.S. Census Bureau geocoding service. The use of third-party services such as this can risk violating privacy rules associated with HIPAA or IRB. At the present time, users should consult their institution's IRB and privacy office before use and consider local geocoding options (e.g., ArcGIS desktop or DeGAUSS) or a HIPAA-compliant service under a business associate agreement (BAA). We are, however, currently modifying the R code so addresses can be geocoded without having to access a third party API to eliminate the potential risk of violating privacy rules.

Functions to aid with geocoding analyses

SVI block group dataset 
2020 Bryan, Michael, 2022, "US Social Vulnerability by Census Block Groups", <https://doi.org/10.7910/DVN/ARBHPK>, Harvard Dataverse, V2, UNF:6:sM/cBUxMDjFYmAdIA/dWBg== [fileUNF]

COI 3.0 overall index and three sub-domains 
diversitydatakids.org. 2024. “Child Opportunity Index 3.0 database, 2020 census tracts”, retrieved from <https://www.diversitydatakids.org/research-library/child-opportunity-index/child-opportunity-index-30-2023-census-tract-data> on Sep 17 2025.

SVI census tract dataset 2022 
Centers for Disease Control and Prevention/ Agency for Toxic Substances and Disease Registry/ Geospatial Research, Analysis, and Services Program. CDC/ATSDR Social Vulnerability Index, 2022, Database, U.S. <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>. Accessed on Jun 06 2024.

ADI census block group dataset 2020-2021 
University of Wisconsin School of Medicine and Public Health. 2020-2021 Area Deprivation Index National & State. Downloaded from <https://www.neighborhoodatlas.medicine.wisc.edu/> on Jun 06 2024.

SDI census tract dataset 2019
Social deprivation index (SDI). Robert Graham Center - Policy Studies in Family Medicine & Primary Care. (2019, November 5). Retrieved Jun 06, 2024, from <https://www.graham-center.org/rgc/maps-data-tools/sdi/social-deprivation-index.html>. 

HIPAA Compliance Development
Currently developing code to address HIPAA compliance and come up with alternative ways to geocode that secures protected health information.
<https://github.com/pltu06/geocoding/issues/4>.
