Tekst "Betrouwbaar betalingsverkeer in Perspectives"

Domein model:Finance als fin: heeft

	zaken

		fin:Transactie heeft
			Properties
				intern
          $eindstandBetaler (Verplicht, Functioneel, Number)
          $eindstandOntvanger (Verplicht, Functioneel, Number)
          $bedrag  (Verplicht, Functioneel, Number)
          $transactieVoltooid (Verplicht, Functioneel, Boolean)
      Rollen
        $Debitor (Functioneel, Verplicht) gevuld door per:Persoon
        $Creditor (Functioneel, Verplicht) gevuld door per:Persoon
        $DebitorWitness (Functioneel, Verplicht) gevuld door psp:Partij
          Properties
            $akkoord (Verplicht, Functioneel, Boolean)
        $CreditorWitness (Functioneel, Verplicht) gevuld door psp:Partij
          Properties
            $akkoord (Verplicht, Functioneel, Boolean)
        $DebitorSuccessorTransaction (Functioneel, Verplicht) gevuld door fin:Transactie
        $CreditorSuccessorTransaction (Functioneel, Verplicht) gevuld door fin:Transactie
        $DebitorPredecessorTransaction (Functioneel, Verplicht) gevuld door fin:Transactie
        $CreditorPredecessorTransaction (Functioneel, Verplicht) gevuld door fin:Transactie
        $CreditorSuccessorTransactionWitness (Functioneel, Verplicht) gevuld door psp:Partij
        $DebitorSuccessorTransactionWitness (Functioneel, Verplicht) gevuld door psp:Partij
      Acties
        $Debitor bindt $DebitorWitness
        $Creditor bindt $CreditorWitness
        $Debitor bindt $DebitorPredecessorTransaction
        $Debitor bindt $DebitorSuccessorTransaction
        $Creditor bindt $CreditorPredecessorTransaction
        $Creditor bindt $CreditorSuccessorTransaction
        $creditor bindt $CreditorSuccessorTransactionWitness
        $debitor bindt $DebitorSuccessorTransactionWitness
        $Debitor betaalt
        $DebitorWitness acoordeert Transactie
        $CreditorWitness acoordeert Transactie
