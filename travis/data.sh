#!/bin/bash

Rscript -e "devtools::load_all('$PWD'); fhidata:::gen_data(file.path('$PWD','inst','createddata'))"
