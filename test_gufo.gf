#The syntax of a test program must be the following:
#a comment line declaring the program number: "#PROGRAM X"
#a comment line describing the program
#The effective program
#and "#END PROGRAM" comment


#return the string corresponding to the program at pos i in the file $file.
let $read_program $i $file = 
  let $sed_expr = "/#PROGRAM "+$i+"$/,/#END PROGRAM/ p" in
  (
   sed -n $sed_expr $file
  )

#return the string corresponding to the program at pos i in the file $file.
let $run_test $curTest $file = 
    let $res = ($read_program ($Int.toString $curTest) $file) in(
    echo "START TEST" ($Int.toString $curTest) $file ; echo $res.Cmd.print > tmpfile; ./guforun.native tmpfile 
    )

#Return the number of programs in a file
let $nb_exemples $file = 
  let $nbCmd = grep "^#PROGRAM" $file | wc -l in
  ($Int.fromString $nbCmd.Cmd.print)

#Run recursively tests in file $file (string) from $cur_test (an integer) to 1.
let $run_tests_until_end_ $cur_test $file =
  #function validing (or invaliding the result of a test)
  let $valid_test $cmd = 
      if ($cmd.Cmd.res != (Some 0))
      then (echo ($cmd.Cmd.print) ; echo "FAILED"; exit)
      else (
        (echo $cmd.Cmd.print; echo "SUCCESS\n" ) )
  in (
    if ($cur_test  == 1 )
    then (
      #termination case: last file
      let $cmd = $run_test 1 $file in
      ($valid_test $cmd)
     )
    else(
      #recursive case: still file to valid
      let $prevcmd = $run_test $cur_test $file in (
      ($valid_test $prevcmd) ;; ($run_tests_until_end_ ($cur_test - 1) $file ))
      )
    )
  
let $run_tests_until_end $file =
  $run_tests_until_end_ ($nb_exemples $file) $file 

**START**


#$run_test 2
($run_tests_until_end "tests/basic_affectations.gf") ;;
($run_tests_until_end "tests/valid_exprs.gf")
