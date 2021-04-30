# Data Science For Non Life Insurance Assignmnet

## Assignment Questions
You analyze the data set (in .csv) that is available on TOLEDO. This data set contains observations on the variables listed in the table printed below. Your report should document the
following steps:
1. An exploratory data analysis.
2. The construction of a (technical) tariff structure for a car insurance product. Hereto you
analyze both the frequency and severity information in the data with (at least) two of
the methods/algorithms discussed in the lectures (GLM, GAM, regression tree, bagging,
random forest, gradient boosting, : : :). You combine frequency and severity models appropriately into a technical pure premium. You compare the performance of the constructed
models, based on your own defined set of criteria. You discuss the resulting (pure premium)
pricing structure.
3. As an extra step you will discuss (and demonstrate) the calculation of a safety (or risk)
loading on top of the pure premiums. To calculate these risk loadings you explore the
literature on insurance pricing and propose a suitable strategy. Yang et al. (2020) is a
useful starting point.
There is no need to answer the above questions separately (question by question) in your report.
A well structured text that covers the above items is preferred. Be creative and rigorous!
## Overview of variables
ageph age of the policyholder
CODPOSS postal code in Belgium
duree exposure, fraction of the year the insured is covered
lnexpo log of exposure
nbrtotc total number of claims during period of exposure
chargtot total claim amount
agecar age of the car: 0 − 1, 2 − 5, 6 − 10, > 10
sexp sex of the policyholder: male or female
fuelc type of fuel: petrol or gasoil
split split of the premium: monthly, once, twice, three times per year
usec use of the car: private or professional
fleetc car belonging to a fleet: yes or no
sportc sport car: yes or no
coverp coverage: MTPL, MTPL+, MTPL+++
powerc power of the car: < 66, 66-110, >110

L. Yang, Z. Li, and S. Meng. Risk loadings in classification ratemaking. https://arxiv.org/
abs/2002.01798, 2020.
