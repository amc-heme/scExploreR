#!/bin/zsh
cd /Users/serveruser/build/macmini_primary
/usr/local/bin/Rscript -e 'shiny::runApp("/Users/serveruser/build/macmini_primary/app.R", host = "192.168.68.111", port = 3830, launch.browser = FALSE)' > server_out.txt 2> server_err.txt

