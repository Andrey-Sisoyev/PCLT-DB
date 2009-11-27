#!/bin/sh

clear
sudo runghc Setup unregister
sudo runghc Setup configure --user
sudo runghc Setup build
sudo runghc Setup haddock
sudo runghc Setup install

echo
echo "###################################"
echo "#"
echo "# Before (or after) installing this Cabal package one should also install DB." 
echo "# " 
echo "# The package uses PostgreSQL 8.4 (not earlier version)."
echo "# Examine and run file ./db/db_inst.sh under user account," 
echo "#     that is used on your system as a PostgreSQL DBMS superuser."
echo "#"
echo "###################################"
