
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

Provides several methods to account for positional errors when spatially
joining ground-truthing with remote sensing patch classifications. The
joined data set is used to estimate the accuracy of patch type (e.g.,
land cover, substrate type, habitat type) classifications derived from
the remote sensing data. The estimated classification accuracy is then
used to reclassify the original patch classifications several times to
account for uncertainty in the accuracy of the original classifications.
The output of allows users to the accuracy and uncertainty of patch
classifications and to propagate classification uncertainty in products
developed from the remote sensing classifications (e.g., habitat maps
and suitabilitiy indicies).

Tasks that can be accomplished with this package include:

- `classificationAccuracy_propagateUncertainty` combines ground-truthing
  data and patch classifications, estimates classification accuracy,
  propagates classification uncertainty
- `summarizeAreas` provides summary statistics and plots of patch types
  reclassified based on estimated classification accuracy

## Installation

``` r
devtools::install_github("USFWS/groundTruther", build_vignettes = T)
```

## Vignette

``` r
vignette(topic = "groundTruther", package = "groundTruther")
```

## Function Flow

<img src="../groundTruther/vignettes/image/groundTrutherWorkflow.png" title="Overview of groundTruther Workflow." alt="Overview of groundTruther Workflow." width="100%" style="display: block; margin: auto;" />

## USFWS Disclaimer

The United States Fish and Wildlife Service (FWS) GitHub project code is
provided on an “as is” basis and the user assumes responsibility for its
use. FWS has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by FWS. The FWS seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by FWS or the United States Government.

## License

This project is licensed under the terms of the [Creative Commons Zero
v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/)
license.
