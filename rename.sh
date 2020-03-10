#!/bin/bash
for old_name in data/*/*/*D*
do
  # Midx=`expr index "$old_name" "M" - 2`
  # par1idx=`expr index "$old_name" "=" + 1`
  # pars=${old_name:$par1idx}
  # mv $old_name ${old_name%%,*}${old_name:$Midx}${pars%%,M*}

  # Didx=`expr index "$old_name" "D" + 4`
  # mv $old_name ${old_name:0:$Didx}${old_name:$Didx+1}
done