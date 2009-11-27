#!/bin/sh

db_inst_log="db_inst.sh.log"
echo -e "\n\n-------------------------------\n-------Running DB script pclt_meta.sql\n" > $db_inst_log
psql -f pclt_meta.sql 2>&1 | cat >> $db_inst_log
echo -e "\n\n-------------------------------\n-------Running DB script pclt.sql (which also calls data.sql)\n" >> $db_inst_log
psql -f pclt.sql 2>&1 | cat  >> $db_inst_log
echo -e "\n\n-------------------------------\n-------Running DB script view.sql\n" >> $db_inst_log
psql -f view.sql 2>&1 | cat  >> $db_inst_log
echo -e "\n\n-------------------------------\n-------Data definition scripts finished.\nIf no explicit errors are found, one may also perform a small test using command\n   psql -f ./local_test/tests.sql 2>&1 | less\n" >> $db_inst_log

echo -e "Examine $db_inst_log file for results." 
 
# data.sql is called from pclt.sql

