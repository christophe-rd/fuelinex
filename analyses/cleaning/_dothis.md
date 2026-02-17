# dothis
## 17 February 2026
In clean measurements, I don't exclude for now height and diameters based on the column "valid" because:
1. I think it's good to keep those punctual measurments for the allometry model
2. There is still instances where the increment between two years is negative despite not having a "yes" in the validity column

Moving forward I think what needs to be done:
1. Create a vector tree_ID_year_heigh (or diameter) for all the instances where the increment is negative. 
2. In the model output, after the allometry, remove those from the dataset, but before fitting the carry-over effect model.