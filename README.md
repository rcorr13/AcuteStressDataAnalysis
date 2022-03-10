# Acute Stress Data Analysis
These are the scripts (mainly written in R) used for the 4 manuscripts included in Rachel Corr's dissertation analyses. 
Because data used are protected, no data files are uploaded publically on git, but information is available upon request.

## Chapter 2 - Stress RLMs
Chapter 2 of my dissertation was previously published in *NeuroImage: Clinical* in a manuscript entitled ["Neural mechanisms of acute stress and trait anxiety in adolescents"](https://doi.org/10.1016/j.nicl.2020.102543).


`MISTStatsNovember_Revised.R` contains the majority of the demographic analyses conducted for this paper, as well as the robust linear models run on extracted ROI data. Graphs featured in Figures 1 and 3 of the above paper were created using the output from the statistics script within `MISTGraphsSeptemberPaper.R`.


Example: Figure 3


![Figure 3](https://ars.els-cdn.com/content/image/1-s2.0-S2213158220303806-gr3.jpg)


## Chapter 3 - Polyvictimization Mediation
Chapter 3 of my dissertation was previously published in *Social Cognitive and Affective Neuroscience* in a manuscript entitled ["Stress-related hippocampus activation mediates the association between polyvictimization and trait anxiety in adolescents"](https://doi.org/10.1093/scan/nsab129). 

Functions used for this analysis are included in `Polyvict_Functions.R`, including the robust mediation model function that utilized the R *robmed* package. Analyses are run using the `PolyvictAnalyses_2021_09_01.R` script. Graphs featured in Figures 2 and 3 of the above paper were created using `GraphPolyvictPaper_2021_09_03.R`.

Example: Figure 3

![Figure 3](https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/scan/PAP/10.1093_scan_nsab129/2/nsab129f3.jpeg?Expires=1649978955&Signature=Nz4dh4XC4aI7dMrXPKfqBDRrlmwQnyjLHW3jBQHxjJ1Wk3f3FFsbfUOvabUUe65lKrkjieccQBnk~UCsdCRnDtrRr1uDdGFx0QQqPdEhVFzx4Oy60jahUO26y1brlDQdIaVRc9XeKdZlYlbqA29gh3UItO8RVOfmUChn2p8WSm7kjCNGNgOMGHJyaoJ4DK6TTygqOgsKdkWhJbv5AKzBH2L~y~Jp7RdOgtqiArOfsXZKvIKU7pDTgiqIAT~9lvtfCwkmVraPaBCEhW4QcHqM8xwsl9rgd~aSwaxqf8xHFc1dDduTMFTLMykOnJd7rnD8ISvCG1Tj-UxNluuoelbmEQ__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA)
## Chapter 4 - Stress Functional Connectivity
Chapter 4 of my dissertation is currently in press at *Biological Psychiatry: Cognitive Neurosceince and Neuroimaging* in a manuscript entitled "Triple Network Functional Connectivity During Acute Stress in Adolescents and the Influence of Polyvictimization". 

The main analyses for this paper were run using the CONN Toolbox (in MATLAB), with a mix of scripting and using the CONN GUI. Demographic analyses were run with `DemographicsMISTFC_20220111.R` and Figure 3 was made with `GraphPolyvictPCC_20220111.R`.

`CONN20_FindingNetworks_Paste.py` uses a tkinter interface to take pasted output of rows of region names from the CONN Toolbox. Then, CONN Atlas information in `FindNetworksKey.xlsx` is used to identify what neural network is associated with that region, and the program prints this information.
For example, pasting
```
64 voxels (86%) covering 2% of atlas.MidFG l (Middle Frontal Gyrus Left)
9 voxels (12%) covering 0% of atlas.SFG l (Superior Frontal Gyrus Left)
1 voxels (1%) covering 0% of atlas.not-labeled
```
into the interface and hitting "Submit" outputs:
```
MidFG L: FPN (64 voxels)
SFG L: FPN (9 voxels)
Not Labeled (1 voxel)
```


## Chapter 5 - Resting-State Functional Connectivity
Chapter 5 of my dissertation is currently under review in a manuscript entitled "Polyvictimization in Adolescents is Associated with Greater Resting-
State Functional Connectivity Between the Central Executive and Salience Networks". 

Similarly to Chapter 4, the main analyses for this paper were run using the CONN Toolbox (in MATLAB), but demographic analyses were run with `rsFC_20211019.R`.


## Additional Notes
Across these files, "PV" or "TotPolyJVQ5" is frequently used to represent polyvictimization and "STAIT" is the State-Trait Anxiety Inventory - Trait scale.


