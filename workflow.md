---
title: "Simulated Landscapes Workflow"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 2
---



In this document we can do the main analysis for the simulated landscapes examples, from simulating the landscapes through to analysing via equations and motifs. Any underlying functions can be saved in the `R` folder, and be called using `source(function_file.R)`. The outline of the workflow is as follows:

1. Simulate landscapes with the following parameters:

      * `p_supply`: proportion of the landscape which is ES supply
      * `p_demand`: proportion of the landscape which is ES demand
      * `f_supply`: the fragmentation level (fractal dimension) of the ES supply (in the range [0, 1] with 1 being the most fragmented)
      * `f_demand`: the fragmentation level (fractal dimension) of the ES demand (in the range [0, 1] with 1 being the most fragmented)
      * `inter`: the interpersion between ES supply and demand (in the range [0, 1] with 1 being the least interspersed)

We will have landscapes for a range of each parameter, and for each parameter combination we will generate 100 replicates.

2. Generate a distance matrix and an attribute data frame from each simulated landscape. The distance matrix will contain Euclidean distances between every supply and demand patch within the landscape (supply-supply, demand-demand, supply-demand). The attribute table will contain the following columns:

      * `ID`: the patch identity to link back to the distance matrix
      * `patch_type`: whether the patch is supply or demand
      * `patch_area`: the size of the patch

    Here we also generate a discrete networks for the network analysis based on the following parameters (we assume no demand-demand links for now):

      * `ee_threshold`: distance threshold for ecological-ecological links (NULL if no ecological-ecological links)
      * `se_threshold`: distance threshold for social-ecological links

3. Calculate ES benefit for the landscape based on the network. This uses general equations for the generation of ES benefits based no the network (see 'benefit_functions.docx' for details). This produces a table with the ES benefits for each replicate. The parameters required are:

Next step is to generate the benefits based on the distance type node type metric. Note here that at present we discretise the network based on a distance threshold for the social-ecological and ecological-ecological links and then calculate the benefit based on this network.

      * `rival`: whether the services are rival (TRUE) or non-rival (FALSE)
      * `alpha`: the rate of production of the potential ecosystem service per unit area at supply nodes (value > 0)
      * `beta`: the influence of connected supply nodes (i.e., the ecological-ecological links) on the rate of production of the potential ecosystem service at supply nodes (value > 0)
      * `gamma`: marginal utility of the service at zero service used
      * `eta`: rate of decline of the marginal utility with increasing use of the service (value > 0)

4. Perform network analysis to characterise network structure

5. Model ES benefits as a function of landscape structure parameters

6. Model ES benefits as function of network structure parameters

# Simulate landscapes

We have created a function `ls_create` which simulates social-ecological landscapes for a given amount and fragmentation of supply and demand, and interspersion between the two. Our simulated landscapes are built up from three neutral landscape models (NLM):

- supply landscape: a fractal Brownian motion NLM where we control the amount (`p_supply`) and configuration (`f_supply`)
- demand landscape: a fractal Brownian motion NLM where we control the amount (`p_demand`) and configuration (`f_demand`)
- a gradient landscape: a planar gradient NLM

The final social-ecological landscape is then created using the following steps:

- merge supply and demand landscapes (equal weighting to both)
- merge the above landscape with the gradient landscape (here, the weighting controls the level of interspersion using the parameter `inter`:

- 1 --> social and ecological patches are as interspersed as the p/f parameters will allow
- 0 --> social and ecological patches are as segregated as the p/f parameters will allow)

For simplicity, we currently are not simulating supply/demand patches.

The initial simulated landscapes will use all combinations of the following range of parameters:

- `p_supply` and `p_demand`: fixed at 0.15
- `f_supply` and `f_demand`: 0, 0.25, 0.5, 0.75, 1 (from low to high fragmentation)
- `inter`: 0, 0.25, 0.5, 0.75, 1 (from low to high interspersion)



The below figure provides an illustration of one iteration of the landscapes:



# Generate distance matrices and networks

Next step is to create the distance matrix, and table which includes `ID`, `patch_code` (0 = neutral, 1 = supply, 2 = demand, 3 = supply/demand), `patch_area` for each simulated landscape. We will also include the parameters used to generate the landscape on the attribute table, for ease of analysis.

Here we also create a discrete social-ecological network based on the distance thresholds between supply-supply nodes (ecological-ecological links) and supply-demand links (social-ecological links). For this we need to define whether ecological-ecological links exist, and the distance thresholds for each type of link, as follows:  

- `ee_link`: TRUE/FALSE indicating presence or absence of ecological-ecological links
- `ee_threshold`: distance threshold for ecological-ecological links
- `se_threshold`: distance threshold for social-ecological links

This is where we also generate the network format for fanmod. For the fanmod file outputs, there are 4 columns (as per the expected fanmod input for coloured vertices): int1 = id of node one, int2 = id of node two, int3 = type of node one, int4 = type of node two. Types are supply, demand, or supply/demand.



The below figure provides an illustration of one iteration of the networks (NOTE NEEDS TO FIXED):



# ES benefit calculation

Next step is to generate the benefits based on the distance type node type metric. Note here that at present we discretise the network based on a distance threshold for the social-ecological and ecological-ecological links and then calculate the benefit based on this network.

Here we assume stylised networks for generic services with flows between supply and demand (see benefit_functions.docx for detailed descriptions of the benefits functions). There are a number of parameters to specify:

- `rival`: whether the services are rival (TRUE) or non-rival (FALSE)
- `alpha`: the rate of production of the potential ecosystem service per unit area at supply nodes (value > 0)
- `beta`: the influence of connected supply nodes (i.e., the ecological-ecological links) on the rate of production of the potential ecosystem service at supply nodes (value > 0)
- `gamma`: marginal utility of the service at zero service used
- `eta`: rate of decline of the marginal utility with increasing use of the service (value > 0)


```
## NULL
## [1] 3 8
## [1] 4 7
## [1] 15 18
## [1] 39 33
## [1] 13 10
## NULL
## [1]  7 13
## [1] 44 79
## [1] 40 74
## [1] 21  7
## [1] 10 17
## [1] 18 18
## [1] 21 17
## [1] 103 101
## [1] 23 51
## [1] 22 19
## [1] 52 53
## [1] 32 42
## [1] 158  58
## [1] 37 35
## [1] 37 28
## [1] 30 40
## [1] 42 60
## [1] 113 111
## NULL
## [1] 15 28
## [1]  8 14
## [1] 23 18
## [1] 36 34
## NULL
## [1] 26 23
## [1] 31 16
## [1] 38 34
## [1] 44 36
## [1] 37 43
## [1] 34 35
## [1] 28 16
## [1] 73 33
## [1] 74 69
## [1] 79 62
## [1] 51 37
## [1] 53 22
## [1] 60 50
## [1]  54 109
## [1] 188 177
## [1] 59 76
## [1] 97 85
## [1] 95 76
## [1] 101 106
## NULL
## [1] 18 10
## [1] 28 12
## [1] 21 20
## [1] 68 65
## [1] 16 15
## [1] 27 16
## [1] 10  9
## [1] 14 24
## [1] 96 52
## [1] 53 32
## [1] 14 17
## [1] 10 17
## [1]  88 114
## [1] 92 96
## [1] 109  84
## [1] 48 71
## [1] 40 54
## [1] 120 117
## [1] 127 165
## [1] 219 220
## [1] 59 53
## [1]  46 101
## [1] 101 129
## [1] 145 160
## NULL
## [1] 11  5
## [1] 24 21
## [1] 78 35
## [1] 82 81
## [1] 7 3
## [1] 19 18
## [1] 34 12
## [1] 15 30
## [1] 91 61
## [1] 33  6
## [1] 23 37
## [1] 10 32
## [1] 96 53
## [1] 136 153
## [1] 98 46
## [1] 48 52
## [1] 14 57
## [1] 121  92
## [1] 146 160
## [1] 86 89
## [1] 112 146
## [1] 145  72
## [1] 133 157
## [1] 248 236
## NULL
## [1] 12  9
## [1] 26 14
## [1] 37 28
## [1] 104  89
## [1]  3 12
## [1] 18  9
## [1] 10 14
## [1] 50 49
## [1] 128  91
## [1] 10 26
## [1] 45 31
## [1] 37 51
## [1] 81 96
## [1] 133 139
## [1] 22 42
## [1] 76 82
## [1] 60 69
## [1] 115 113
## [1] 204 196
## [1] 83 93
## [1] 151 128
## [1] 117 138
## [1] 192 163
## [1] 264 262
```
# Network analysis



# Modelling ES benefits based on landscape structure



# Modelling ES benefits based on network structure


