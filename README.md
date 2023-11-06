# scExploreR

<br>

*Welcome to the single cell visualization tool you didn't know you were looking for!* This app is designed to make exploration of highly complex data sets easy for anyone, regardless of informatics background. Whether you're a researcher looking to make sense of your single cell data, or a bioinformatician looking to present your results interactively, you will find value in this app. scExplorer bridges the gap between domain (disease and clinical) specific knowledge and high level sequencing data, and ultimately makes the best use of these precious data sets to glean molecular and translational insights through democratizing analysis. 

## Requirements

<br>

* A pre-processed and finalized single cell object (or a set of finalized objects). scExploreR can except most common single cell data formats! The currently supported formats are Seurat, SingleCellExperiment, and Anndata.
* A server to host the app. This can be any computer with at that can be left on and be connected to the internet continuously. RAM requirements vary depending on the object type.
<!-- Add page on HDF5 storage, and put a link here -->
  <!-- For Seurat objects, you need at least as much RAM as the size of the object in memory, but the size of the object can be considerably greater than the available RAM for Anndata and SingleCellExperiment objects using HDF5 storage. -->

Some bioinformatics experience is required to install the app and configure each Seurat object, but once set up, the app can be used by anyone.

<br>

## Installation and Use

1. Install from Github via
```
remotes::install_github("amc-heme/scExploreR")
```
2. Process or obtain a finalized single cell object.

3. Configure an object for the browser by using the configuration app provided with the package. For more information on the config app, see the [config app documentation]().
```
scExploreR::run_config(
  object_path = "path_to_your_seurat_object.rds",
  # The config path will be blank the first time you use the config app for an object
  config_path = "previously_loaded_config_file"
  )
```

4. To set up a browser for others to use, create a browser config YAML file (If you are using the browser locally for your own use, skip to step 4). The file will contain a list of datasets with the path to the objects and config files for each, along with browser specific settings. See [browser config setup]() for more info.

5. Run scExploreR. There are multiple ways to do this: 

<ul>
  <li>
  If setting up a browser, use the path to your config file.
  
  ```
  scExploreR::run_scExploreR(
    browser_config = "./config.yaml"
    )
  ```
  
  </li>
  <li>
  If setting up locally, enter the path to your object and object-specific config file. Only one object may be viewed at once using this method. 
  
  ```
  scExploreR::run_scExploreR(
    object_path = "./object.rds"
    config_path = "./config.yaml"
    )
  ```
  
  </li>
</ul>
<br>



## Future Goals

<br>

<!-- As stated above, the current version of the app requires manually fitting each new object to its own specific version of the app. Future versions of the app will be able to accept *any* Seurat object, automatically detect (or user specified) metadata values of interest, and build the app to provide exploration of that object. 

<br>
-->

* Additional analyses such as GSEA will be added in the future
* Explicit support for single cell data besides CITE-seq will be added.

<br>

