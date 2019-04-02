#!/bin/bash

Rscript -e "devtools::load_all('$PWD'); print('$PWD'); fhidata:::gen_data_all('$PWD/data')"
