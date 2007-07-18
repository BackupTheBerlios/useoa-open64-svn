rm -f temp.dot temp2.dot temp3.dot
cp -f $argv[1] temp.dot
sed -e s/:0:.predef_I4//g -e s/:0:.predef_I8//g -e s/:0:.predef_F4//g -e s/:0:.predef_F8//g -e s/:0:anon_ptr.//g -e s/0://g temp.dot > temp2.dot
sed -e s/"  ICFG"//g -e s/"CALL_RETURN"/"C_R"/g -e s/"_NODE"/"_N"/g -e s/"CALL_("/"C("/g -e s/"RETURN_("/"R("/g temp2.dot > temp3.dot
cp -f temp3.dot $argv[1]
rm -f temp.dot temp2.dot temp3.dot
