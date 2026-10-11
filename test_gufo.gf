#The syntax of a test program must be the following:
# - a comment line declaring the program number: "#PROGRAM X"
# - a comment line describing the program
# - the effective program 
# - and "#END PROGRAM" comment

let $gufo_exe = "_build/default/src/guforun.exe"

#return the string corresponding to the program at pos i in the file $file.
let $read_program $i $file =
  #Return a string
  let $sed_expr = "/#PROGRAM "^$i^"$/,/#END PROGRAM/ p" in
  (
    sed -n $sed_expr $file
  )

#return a program execution command result.
let $run_test $curTest $file = 
    let $res = ($read_program ($Int.toString $curTest) $file) in (
        echo "START TEST" ($Int.toString $curTest) $file ; echo $res.Cmd.print > tmpfile;
        ./_build/default/src/guforun.exe tmpfile 
        )

#Return the number of programs in a file
let $nb_exemples $file = 
  let $nbCmd = grep "^#PROGRAM" $file | wc -l in
  ( $Opt.get ($Int.fromString $nbCmd.Cmd.print) 0)

#Run recursively tests in file $file (string) from $cur_test (an integer) to 1.
#All the tests here, are expected to be valid.
let $run_valid_tests_ $cur_test $file =
  #function validing (or invaliding the result of a test)
  let $valid_test $test_number $cmd = 
      if ($cmd.Cmd.res != (Some 0))
      then (echo ($cmd.Cmd.print) ; echo "FAILED"; exit)
      else (echo ("Test " ^ ($Int.toString $test_number) ^ "succeded"))
  in (
    if ($cur_test  == 1 )
    then (
      #termination case: last file
      let $cmd = $run_test 1 $file in
      ($valid_test $cur_test $cmd)
     )
    else(
      #recursive case: still file to valid
      let $prevcmd = $run_test $cur_test $file in (
      ($valid_test $cur_test $prevcmd) ;; ($run_valid_tests_ ($cur_test - 1) $file ))
      )
    )

#let $run_error_tests_ $cur_test $file = 
#  #function validing (or invaliding the result of a test)
#  #we want to check that gufo raised an error but also that this error occured
#  #before the gufoEngine part.
#  let $error_test $cmd = 
#      if ($cmd.Cmd.res == (Some 0))
#      then (echo ($cmd.Cmd.print) ; echo "FAILED(should have been invalid)"; exit)
#      else (
#        $cmd.Cmd.print_err
#        (echo $cmd.Cmd.print; echo "SUCCESS(invalid as expected)\n" ) 
#        )
#  in (
#    if ($cur_test  == 1 )
#    then (
#      #termination case: last file
#      let $cmd = $run_test 1 $file in
#      ($error_test $cmd)
#     )
#    else(
#      #recursive case: still file to valid
#      let $prevcmd = $run_test $cur_test $file in (
#      ($error_test $prevcmd) ;; ($run_valid_tests_ ($cur_test - 1) $file ))
#      )
#    )




  
let $run_valid_tests $file =
  (echo ("Running test " ^ $file));;
  ($run_valid_tests_ ($nb_exemples $file) $file)

**START**


#$run_test 2
($run_valid_tests "tests/basic_affectations.gf") ;;
($run_valid_tests "tests/valid_exprs.gf") ;;
($run_valid_tests "tests/valid_commands.gf") ;;
($run_valid_tests "tests/basic_operation.gf")
