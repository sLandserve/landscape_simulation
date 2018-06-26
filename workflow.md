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
      * `f_supply`: the fragmentation level (fractal dimension) of the ES supply (in the range [0, 2] with 2 being the least fragmented)
      * `f_demand`: the fragmentation level (fractal dimension) of the ES demand (in the range [0, 1] with 1 being the least fragmented)
      * `inter`: the interpersion between ES supply and demand (in the range [0, 1] with 1 being the least interspersed)

We will have landscapes for a range of each parameter, and for each parameter combination we will generate 100 replicates.

2. Generate a distance matrix and an attribute data frame from each simulated landscape. The distance matrix will contain Euclidean distances between every supply and demand patch within the landscape (supply-supply, demand-demand, supply-demand). The attribute table will contain the following columns:

      * `ID`: the patch identity to link back to the distance matrix
      * `patch_type`: whether the patch is supply or demand
      * `patch_area`: the size of the patch

    Here we also generate a discrete networks for the network analysis based on the following parameters (we assume no demand-demand links for now):

      * `ee_link`: TRUE/FALSE indicating presence or absence of ecological-ecological links
      * `ee_threshold`: distance threshold for ecological-ecological links
      * `se_threshold`: distance threshold for social-ecological links

3. Calculate ES benefit for the landscape based on the network. This uses general equations for the generation of ES benefits based no the network (see 'benefit_functions.docx' for details). This produces a table with the ES benefits for each replicate. The parameters required are:

Next step is to generate the benefits based on the distance type node type metric. Note here that at present we discretise the network based on a distance threshold for the social-ecological and ecological-ecological links and then calculate the benefit based on this network.

      * `rival`: whether the services are rival (TRUE) or non-rival (FALSE)
      * `alpha`: the rate of production of the potential ecosystem service per unit area at supply nodes (value > 0)
      * `beta`: the influence of connected supply nodes (i.e., the ecological-ecological links) on the rate of production of the potential ecosystem service at supply nodes (value > 0)
      * `eta`: the number of beneficiaries per unit area (value > 0)
      * `lambda`: the utility per unit of ecosystem service used (value > 0).  

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

- 0 --> social and ecological patches are as interspersed as the p/f parameters will allow
- 1 --> social and ecological patches are as segregated as the p/f parameters will allow)

For simplicity, we currently are not simulating supply/demand patches.

The initial simulated landscapes will use all combinations of the following range of parameters:

- `p_supply` and `p_demand`: fixed at 0.15
- `f_supply` and `f_demand`: 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0 (from high to low fragmentation)
- `inter`: 0, 0.25, 0.5, 0.75, 1



The below figure provides an illustration of one iteration of the landscapes:

![](workflow_files/figure-html/plot_ls-1.png)<!-- -->

# Network, distance and attribute tables

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
- `eta`: the number of beneficiaries per unit area (value > 0)
- `lambda`: the utility per unit of ecosystem service used.  


```
## [1] 192  35
## [1] 180  52
## [1] 152  57
## [1] 102  55
## [1] 63 63
## [1] 172  38
## [1] 176  27
## [1] 88 62
## [1] 87 55
## [1] 55 62
## [1] 176  38
## [1] 179  28
## [1] 52 69
## [1] 72 50
## [1] 52 55
## [1] 102  53
## [1] 111  45
## [1] 109  48
## [1] 57 62
## [1] 88 40
## [1] 80 61
## [1] 67 50
## [1] 140  44
## [1] 83 48
## [1] 44 43
## [1] 64 45
## [1] 83 59
## [1] 148  44
## [1] 87 51
## [1] 38 53
## [1] 71 76
## [1] 56 65
## [1] 35 61
## [1] 39 31
## [1] 36 41
## [1] 157  35
## [1] 150  37
## [1] 113  62
## [1] 85 56
## [1] 53 59
## [1] 140  29
## [1] 131  41
## [1] 116  51
## [1] 37 65
## [1] 35 48
## [1] 97 28
## [1] 53 51
## [1] 79 47
## [1] 64 39
## [1] 49 47
## [1] 59 46
## [1] 88 34
## [1] 83 25
## [1] 39 41
## [1] 31 42
## [1] 92 28
## [1] 42 33
## [1] 55 43
## [1] 45 42
## [1]  9 24
## [1] 107  37
## [1] 67 47
## [1] 59 32
## [1] 66 47
## [1] 23 40
## [1] 86 41
## [1] 33 34
## [1] 49 54
## [1] 16 38
## [1] 31 40
## [1] 141  43
## [1] 91 57
## [1] 83 53
## [1] 83 56
## [1] 66 47
## [1] 98 37
## [1] 93 39
## [1] 85 47
## [1] 66 47
## [1] 67 43
## [1] 60 39
## [1] 68 34
## [1] 56 35
## [1] 44 36
## [1] 21 35
## [1] 36 30
## [1] 31 24
## [1] 37 27
## [1] 52 38
## [1] 27 43
## [1] 39 31
## [1] 67 22
## [1] 50 37
## [1] 32 31
## [1] 27 15
## [1] 35 14
## [1] 48 44
## [1]  7 20
## [1] 14 26
## [1] 29 34
## [1] 25 26
## [1] 40 32
## [1] 20 34
## [1] 22 17
## [1]  6 16
## [1] 106  39
## [1] 107  33
## [1] 143  38
## [1] 35 40
## [1] 95 59
## [1] 108  39
## [1] 82 39
## [1] 28 29
## [1] 40 36
## [1] 63 34
## [1] 49 37
## [1] 63 28
## [1] 90 41
## [1] 57 24
## [1] 13 17
## [1] 36 34
## [1] 28 25
## [1] 10 31
## [1] 29 30
## [1] 26 29
## [1] 22 16
## [1] 31 26
## [1] 32 28
## [1] 10 24
## [1] 33 19
## [1] 17 23
## [1] 27 21
## [1] 17 21
## [1] 53 17
## [1] 34 17
## [1] 12 25
## [1] 16 21
## [1]  6 12
## [1] 10 13
## [1]  7 18
## [1] 70 61
## [1] 67 47
## [1] 26 47
## [1] 72 51
## [1] 78 58
## [1] 61 32
## [1] 37 56
## [1] 46 48
## [1] 37 47
## [1] 69 38
## [1] 35 34
## [1] 58 32
## [1] 35 33
## [1]  6 25
## [1] 24 41
## [1] 70 25
## [1] 13 22
## [1] 25 24
## [1] 22 19
## [1] 26 13
## [1] 13 22
## [1] 10  9
## [1] 25 18
## [1]  5 18
## [1]  9 20
## [1] 28 14
## [1]  9 14
## [1] 10 11
## [1] 3 6
## [1] 16  9
## [1] 3 5
## [1] 17  8
## [1] 36 13
## [1] 20 22
## [1] 10 18
## [1] 61 63
## [1] 99 50
## [1] 51 65
## [1] 104  67
## [1] 82 42
## [1] 56 45
## [1] 65 33
## [1] 38 51
## [1] 50 44
## [1] 46 31
## [1] 65 38
## [1] 57 39
## [1] 34 18
## [1] 16 26
## [1]  5 15
## [1] 26 29
## [1] 24 20
## [1] 15 14
## [1] 26 17
## [1] 28 25
## [1] 11 11
## [1] 14 21
## [1] 15 12
## [1]  3 16
## [1] 9 8
## [1] 20 18
## [1] 10 14
## [1] 11 13
## [1] 10 11
## [1] 11 14
## [1] 8 7
## [1] 8 8
## [1] 8 9
## [1] 6 7
## NULL
## [1] 66 70
## [1] 66 64
## [1] 99 50
## [1] 32 55
## [1] 28 32
## [1] 43 44
## [1] 107  47
## [1] 79 52
## [1] 77 41
## [1] 43 33
## [1] 30 28
## [1] 24 35
## [1] 36 42
## [1] 34 38
## [1] 26 28
## [1] 16 21
## [1] 8 7
## [1] 25 28
## [1] 38 18
## [1]  9 13
## [1] 11  6
## [1] 36 11
## [1] 19 14
## [1] 24 16
## [1] 15 23
## [1] 28  7
## [1] 6 8
## [1] 15 14
## [1]  5 12
## [1] 9 5
## NULL
## [1] 8 4
## [1] 7 5
## [1] 10  8
## NULL
## [1] 213  20
## [1] 195  57
## [1] 129  46
## [1] 109  66
## [1] 113  47
## [1] 190  43
## [1] 127  41
## [1] 89 44
## [1] 96 51
## [1] 80 50
## [1] 131  38
## [1] 84 30
## [1] 110  42
## [1] 82 70
## [1] 69 62
## [1] 131  39
## [1] 66 76
## [1] 93 54
## [1] 59 54
## [1] 98 43
## [1] 92 56
## [1] 86 57
## [1] 133  53
## [1] 54 58
## [1] 45 47
## [1] 89 51
## [1] 109  64
## [1] 88 36
## [1] 37 38
## [1] 33 46
## [1] 96 56
## [1] 80 54
## [1] 96 60
## [1] 58 63
## [1] 31 41
## [1] 163  33
## [1] 127  38
## [1] 146  32
## [1] 112  48
## [1] 46 55
## [1] 134  41
## [1] 121  46
## [1] 60 57
## [1] 65 66
## [1] 69 54
## [1] 113  32
## [1] 96 35
## [1] 48 55
## [1] 98 41
## [1] 75 32
## [1] 80 45
## [1] 88 29
## [1] 45 50
## [1] 84 63
## [1] 97 20
## [1] 68 50
## [1] 86 38
## [1] 94 25
## [1] 41 47
## [1] 36 33
## [1] 83 29
## [1] 80 34
## [1] 38 34
## [1] 40 41
## [1] 28 55
## [1] 107  71
## [1] 54 34
## [1] 50 20
## [1] 91 52
## [1] 17 28
## [1] 126  41
## [1] 127  32
## [1] 83 48
## [1] 86 53
## [1] 29 50
## [1] 104  37
## [1] 105  38
## [1] 124  25
## [1] 53 58
## [1] 27 47
## [1] 74 43
## [1] 72 46
## [1] 63 49
## [1] 104  37
## [1] 27 31
## [1] 73 24
## [1] 52 37
## [1] 77 32
## [1] 17 26
## [1] 18 17
## [1] 27 29
## [1] 57 25
## [1] 30 32
## [1] 21 23
## [1] 20 35
## [1] 60 27
## [1] 40 30
## [1] 44 26
## [1] 55 31
## [1] 32 25
## [1] 57 38
## [1] 45 28
## [1] 24 26
## [1] 61 25
## [1] 42 36
## [1] 105  61
## [1] 135  47
## [1] 73 66
## [1] 82 52
## [1] 71 44
## [1] 84 31
## [1] 59 49
## [1] 51 51
## [1] 71 40
## [1] 48 42
## [1] 80 23
## [1] 37 41
## [1] 32 38
## [1] 45 38
## [1] 27 24
## [1] 37 22
## [1] 42 33
## [1] 84 28
## [1] 22 40
## [1]  6 15
## [1] 27 26
## [1] 20 12
## [1] 19 20
## [1]  8 18
## [1] 27 23
## [1] 18 17
## [1] 38 30
## [1] 50 26
## [1] 20 15
## [1] 22 13
## [1] 46 23
## [1] 22 26
## [1] 17 24
## [1] 12  5
## [1] 39 21
## [1] 100  50
## [1] 38 50
## [1] 148  50
## [1] 60 56
## [1] 42 36
## [1] 81 35
## [1] 113  32
## [1] 46 37
## [1] 85 46
## [1] 16 32
## [1] 48 33
## [1] 66 27
## [1] 28 28
## [1] 38 34
## [1] 37 22
## [1] 62 30
## [1] 41 19
## [1] 18 17
## [1] 51 21
## [1] 10  7
## [1] 27 16
## [1] 14 10
## [1] 35 21
## [1]  6 17
## [1] 5 4
## [1] 18 18
## [1]  7 12
## [1] 5 4
## [1] 24 12
## [1] 20  8
## [1] 34 19
## [1] 15 26
## [1] 20  9
## [1] 11 12
## [1] 4 9
## [1] 100  44
## [1] 117  54
## [1] 112  59
## [1] 98 68
## [1] 58 42
## [1] 69 41
## [1] 87 39
## [1] 42 41
## [1] 22 41
## [1] 90 44
## [1] 48 22
## [1] 37 29
## [1] 16 25
## [1] 30 31
## [1] 15 32
## [1] 37 24
## [1] 19 20
## [1] 17 12
## [1] 12 10
## [1] 11 16
## [1] 11 12
## [1]  6 16
## [1] 15 13
## [1] 10 10
## [1] 7 2
## [1] 19  9
## [1] 4 2
## [1] 2 7
## [1] 18  9
## [1] 2 8
## [1] 17 11
## [1] 17 16
## [1] 5 8
## [1]  8 10
## [1] 13  7
## [1] 93 72
## [1] 97 56
## [1] 133  48
## [1] 39 62
## [1] 36 22
## [1] 24 60
## [1] 69 54
## [1] 26 38
## [1] 43 32
## [1] 41 30
## [1] 23 28
## [1] 17 14
## [1] 26 27
## [1] 38 22
## [1] 11 28
## [1] 18 16
## [1] 18 19
## [1] 39 29
## [1] 13 21
## [1]  9 14
## [1] 40 16
## [1] 21 14
## [1] 9 9
## [1]  5 15
## [1] 2 8
## [1] 4 5
## [1] 8 7
## [1] 3 6
## [1] 4 2
## [1]  5 13
## [1] 6 5
## [1] 3 7
## [1] 15  5
## [1] 20  5
## [1] 6 3
```
# Network analysis



# Modelling ES benefits based on landscape structure



# Modelling ES benefits based on network structure


