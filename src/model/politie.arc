Tekst "representatief voorbeeld van ARC" -- Deze regel geeft de instantie van GepresenteerdContextType van de omhullende context zijn naam
{- 
Deze tekst heeft alle mogelijke expressies die de ARC syntax toelaat.
-}
Omhullende Context is Domein model:Politie$ --Commentaar achter de context declaratie.

zaken

-- Dit commentaar staat boven de zaak Aangifte.
Zaak Aangifte heeft -- Dit commentaar staat op de regel die Aangifte declareert.
	import model:PersoonlijkDomein als per:
	properties
		-- Dit commentaar staat boven de sectie intern van Aangifte.
		intern -- Dit commentaar staat achter het sleutelwoord 'intern'.
			urgentie (String, Verplicht, Niet Functioneel)
			-- Dit commentaar staat onderin de sectie 'intern' van Aangifte.
		extern
			aantekening (String, Niet Verplicht, Functioneel)
	rollen
		UserRol (Functioneel) gevuld door per:Persoon heeft
			properties
				betrouwbaarheid (Number, Verplicht, Functioneel)
			views
				adres met properties
					-- hieronder alle properties van de view.
					betrouwbaarheid -- Dit is een property van de rol.
		Verbalisant (Functioneel) gevuld door Medewerker
		Start (Functioneel) gevuld door Intake heeft
			properties
				toegewezen (Boolean, Niet Verplicht, Functioneel)
	activiteiten
		Intake heeft
			rollen
				Uitvoerder (Functioneel) gevuld door Verbalisant
	acties
		Verbalisant beheert locatiegegevens van Verbaal
