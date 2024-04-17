!/bin/sh
redshift -p | grep -oP "(?<=Period:).*"
