echo "Running tests..."
if Rscript --vanilla ./test1.R ; then
 echo "Pass: Program exited zero"
else
 echo "Fail: Program did not exit zero"
 exit 1
fi
echo "All tests passed."
exit 0
