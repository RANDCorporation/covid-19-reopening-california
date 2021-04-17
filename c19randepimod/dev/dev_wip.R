
#
attachment::att_amend_desc()
devtools::document()
devtools::build()

devtools::load_all()

# Development WIP File:

library(c19randepimod)

# Build PDF Manual
devtools::build_manual()

# Build Vignettes;
devtools::build_vignettes()


augm_inputs = get_augmented_inputs(inputs_path = "../app/data/inputs", write_to_server = F, )

# Development Tasks:s

# Create RDM Model With Single "Future Uncertainty"

# Run Calibrated Model Over Many Futures using these Uncertainties

# Compute Regret for Each Strategy

# Show Two Key Parameters and Regret

# Run PRIM Analysis

# Iterate Over Model.


setup_c19_folder(path = "./testpath")
