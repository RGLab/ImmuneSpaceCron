 #!/bin/bash
set -ev

R CMD build .
R CMD check ImmuneSpaceCronjobs*tar.gz
