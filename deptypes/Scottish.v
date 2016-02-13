Section ScottishClub.
 Variables Scottish RedSocks WearKilt Married GoOutSunday : Prop.
 Hypothesis rule1 : ~ Scottish -> RedSocks.
 Hypothesis rule2 : WearKilt \/ ~ RedSocks.
 Hypothesis rule3 : Married -> ~ GoOutSunday.
 Hypothesis rule4 : GoOutSunday <-> Scottish.
 Hypothesis rule5 : WearKilt -> Scottish /\ Married.
 Hypothesis rule6 : Scottish -> WearKilt.

 Lemma no_member : False. tauto. Qed.
End ScottishClub.
(* Variables and Hypotheses will we discarded, the Lemma will be generilized *)
