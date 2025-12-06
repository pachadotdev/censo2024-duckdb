#!/bin/bash

# divide censo2024_corregido.duckdb
# let's put it in a ZIP file organized in 10 fragments for GitHub limits

zip -s 300m censo2024_corregido.zip censo2024_corregido.duckdb
