# An iNaturalist-pl@ntNet-workflow to identify plant-pollinator interactions – a case study of *Isodontia mexicana*

During the Alien-CSI Hackathon in Romania, which focused on ecological interactions of invasive species, the Coding Club idea of deriving information about plant-pollinator interactions from iNaturalist images was further developed. *Isodontia mexicana*, native to North America and introduced to Europe, was chosen as an example species. The objective was to see if it is possible to learn more about which plant species, genera, and families are visited by *I. mexicana* from images of the insects on plants.

![A photograph of *Isodontia mexicana* visiting a flower, picture from iNaturalist (https://www.inaturalist.org/photos/173385904) from its invasive range in France. ©ahmedm, used under the CC-By license](data/Isomex_image.jpeg)

The general approach of this idea is, to retrieve research grade quality image URLs from iNaturalist and feed these URLs as a bunch into the plant identification app [Pl@ntNet](https://plantnet.org/en/). Plantnet returns for each imageURL a list of candidate plants with a corresponding score between 0.001 (lowest) and 1 (pretty sure about that plant).

