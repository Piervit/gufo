let $init_votes = -< "M. Dupond" : 0 , "Mme. Michoux" : 0, "M.Dudulle" : 0>-

let $init_listVoted = -<>-

#update the vote for a candidate if $candidat represent a valid candidate.
let $incrvote $votes $candidat = 
  let $votecandidat = ($Map.get $votes $candidat) in(
    if ($votecandidat == None )
    then ($votes) #return votes without updating it.
    else ($Map.add $votes $candidat (($Opt.force $votecandidat) + 1 ) )
  )
  
let $funvote $votes $listVoted $codevotant $candidat = 
  if($Set.is_in $listVoted $codevotant) 
  then (False -- $listVoted -- $votes ) 
  else(
    let $listVoted = $Set.add $listVoted $codevotant in (
    let $votes = $incrvote $votes $candidat in(
      True -- $listVoted -- $votes
    )
    )
  )



