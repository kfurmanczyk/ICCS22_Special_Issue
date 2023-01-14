The files in this repository are the source code accompanying the paper

"Classification and Feature Selection Methods
Based on Fitting Logistic Regression to PU Data"

The scripts dependencies are as follows:

datasets_prep.R; prep_functions.R
datasets downloading and basic preprocessing

scoring_methods.R
coding (or wrapping) our ML schemes

You need use above scripts present to perform experiments connected with PU issues and collect results. You can perform:


PU_script_LassoJoint.R
experiments connected with LassoJoint itself and variables selection in LassoJoint

PU_script_delta_testing.R
experiments connected with Delta-parameter in LassoJoint schemes

PU_script_MIFJoint.R
experiments connected with MIFJoint itself and variables selection in MIF

PU_script_all_Ada.R
experiments connected with AdaSampling


If you store all script in the same working directory you can simply run one of one script prefixed "PU_script" to perform part of the research you are interested in. You can find additional comments (about script structure) in PU_script_LassoJoint.R






