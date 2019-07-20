Tekst "representatief voorbeeld van ARC"

Domein model:Politie heeft

	zaken

		Aangifte heeft
			import model:PersoonlijkDomein als per:
			properties
					urgentie (String, Verplicht, Niet Functioneel)
					aantekening (String, Niet Verplicht, Functioneel)
			rollen
				UserRol (Functioneel, Verplicht) gevuld door per:Persoon heeft
					properties
						naam (String, Verplicht, Functioneel)
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
