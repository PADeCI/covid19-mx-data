![Build Status](https://img.shields.io/github/issues/PADeCI/covid19-mx-data)
![Build Status](https://img.shields.io/github/forks/PADeCI/covid19-mx-data)
![Build Status](https://img.shields.io/github/stars/PADeCI/covid19-mx-data)
![Build Status](https://img.shields.io/github/license/PADeCI/covid19-mx-data)
![Build Status](https://img.shields.io/twitter/url?style=social)

<p align="center">
<img src = "https://github.com/PADeCI/covid19-mx-data/blob/master/logo.png" alt="logo" width="200"/>
</p> 

_____
# VERSIÓN EN ESPAÑOL: Datos de COVID-19 en Mexico 
# Sobre este repositorio :open_book:
[PADeCI](https://twitter.com/PADeCI1) es un equipo de investigación interdisciplinario ubicado en el CIDE Región Centro Aguascalientes, México. Actualmente, uno de sus principales proyectos involucra el procesamiento, el análisis y la creación de datos sobre COVID-19 en México, necesarios para la toma de decisiones basada en evidencia en contextos de incertidumbre. Este repositorio corresponde a la primera etapa del análisis de datos: el proceso de limpieza. Los datos que son generados en esta etapa son la fuente primaria de información para el modelaje matemático, las proyecciones y las publicaciones de PADeCI. Siguiendo el principio científico de _transparencia_, cualquier persona interesada puede replicar el proceso de limpieza de datos hecho por el equipo de PADeCI, usando el contenido de este repositorio.

# Requisitos :computer:
R version 3.6.2 (ésta es la versión recomendada para evitar errores en el código, también conocidos como _bugs_). 

# Uso :inbox_tray:
Cualquier persona puede replicar el trabajo de PADeCI, ya sea clonando este repositorio en su computadora o descargando archivos específicos. 

# Descripción de las carpetas :card_index_dividers:
**1. Analysis:** Esta carpeta contiene dos archivos de código en R que lidian con la limpieza de datos a nivel municipal y a nivel estatal (aquí se incluye también información nacional y de la Zona Metropolinata del Valle de México o _ZMVM_).

**2. Automatization:** La carpeta de de automatización incluye varios archivos de código en R que automatizan tareas tales como descargar los datos oficiales, compararlos con los datos generados por los códigos de limpieza e incluso generar reportes diarios, entre otros.

**3. Data raw:** Aquí se guarda la información cruda a partir de la cual se construyen las bases de datos que son la fuente principal para todos los productos de PADeCI. Se trata de las bases diarias que la Secretaría de Salud sobre número de casos sospechosos y confirmados en el país. También incluye datos poblacionales necesarios para el modelaje estadístico.

**4. Data validation:** La validación de los datos consiste en registros diarios tanto de la información reportada por las autoridades mexicanas, así como de los datos generados por el proceso de limpieza de PADeCI.

**5. Data:**  La carpeta de datos contiene todas las bases limpias, las cuales incluyen:
- Información a nivel municipal (actualizada semanalmente)
- Información a nivel estatal (actualizada diariamente)
  - Incluye datos nacionales 
  - También información de la Zona Metropolitana del Valle de México (ZMVM)

# Autoras y autores :writing_hand:
Fernando Alarid-Escudero   | [GitHub](https://github.com/feralaes) | [Twitter](https://twitter.com/feralaes) |

Yadira Peralta             | [GitHub](https://github.com/yadira-peralta) | [Twitter](https://twitter.com/YadiraPerTor) |

Mariana Fernández          | [GitHub](https://github.com/marianafdz465) | [Twitter](https://twitter.com/marianafdz_97) |

Andrea Luviano             | [GitHub](https://github.com/AndreaLuviano) | [Twitter](https://twitter.com/AndreaLuviano) |

Manuel Cardona             | [GitHub](https://github.com/manucardona) | [Twitter](https://twitter.com/ManiArias) |

Hirvin Díaz                | [GitHub](https://github.com/HirvinDiaz) | [Twitter](https://twitter.com/HazaelDiaz93) |

Regina Isabel Medina       | [GitHub](https://github.com/RMedina19) | [Twitter](https://twitter.com/regi_medina) |

_____

# ENGLISH VERSION: Mexico's COVID-19 data
# About this repository :open_book:
[PADeCI](https://twitter.com/PADeCI1) is an interdisciplinary research team based in Aguascalientes, Mexico. Currently, one of it's main projects concerns the processing, analysis and creation of COVID-19 data for Mexico, necessary for evidence-based decision making amid the context of uncertainty. This repository concerns the first stage of data analysis: the cleaning process. The data that is generated at this stage is the primary source of information that is used in PADeCI's mathematical modeling,  projections and publications. Following the scientific principle of _transparency_, any person can replicate the cleaning process done by PADeCI's team using the contents of this repository. 

# Requirements :computer:
R version 3.6.2 (this version is recommended to avoid potential bugs) 

# Usage :inbox_tray:
Any user can either clone this repository in its own computer or download specific files in order to replicate PADeCI's job.

# Folders' description :card_index_dividers:
**1. Analysis:** This folder containts two R scripts which manage data wranglig at county level and state level (which includes nation wide information as well as Mexico City Metropolitan Area, or _MCMA_, data). 

**2. Automatization:** The automatization folder include several scripts that automate tasks such as downloading official data and comparing it with the data generated by the wranging scripts and even generating daily reports, among others.  

**3. Data raw:** This folder stores the raw data from which cleaned data bases are constructed, these are the main source for every product published by PADeCI. It mainly is constituted by the open source data bases issued daily by the Mexican Ministry of Healt (_Secretaría de Salud_) that report the number of COVID-19 suspicious and confirmed cases in the country. It also includes population data necessary to posterior statistical modeling. 

**4. Data validation:** Data validation consist in keeping a daily register of both, data reported by Mexican authorities and PADECI's cleaned data.  

**5. Data:** The data folder contains all the cleaned data bases, which include: 
- County level data (issued weekly) 
- State level data (issued daily) 
  - It includes national data
  - It also includes Mexico City Metropolitan Area (MCMA) data

# Authors :writing_hand:
Fernando Alarid-Escudero   | [GitHub](https://github.com/feralaes) | [Twitter](https://twitter.com/feralaes) |

Yadira Peralta             | [GitHub](https://github.com/yadira-peralta) | [Twitter](https://twitter.com/YadiraPerTor) |

Mariana Fernández          | [GitHub](https://github.com/marianafdz465) | [Twitter](https://twitter.com/marianafdz_97) |

Andrea Luviano             | [GitHub](https://github.com/AndreaLuviano) | [Twitter](https://twitter.com/AndreaLuviano) |

Manuel Cardona             | [GitHub](https://github.com/manucardona) | [Twitter](https://twitter.com/ManiArias) |

Hirvin Díaz                | [GitHub](https://github.com/HirvinDiaz) | [Twitter](https://twitter.com/HazaelDiaz93) |

Regina Isabel Medina       | [GitHub](https://github.com/RMedina19) | [Twitter](https://twitter.com/regi_medina) |


