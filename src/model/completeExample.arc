

	Context : Domain : Feestdomain
		Role : Role : Cadeauwens
			Mandatory : False
			NonFunctional : True
			Calculated : False
			FilledBy : Host
			FilledBy : Gast
		Role : ExternalRole : ExternalRole of Feestdomain
		Role : Role : Feest
			Mandatory : False
			NonFunctional : True
			Calculated : False
			FilledBy : Gast
			FilledBy : Host
		Agent : UserRole : Gast
			Mandatory : False
			Calculated : False
			Perspective : LocalPerspective : ContextScreen of Gast
				ScreenLocationX : 100
				ScreenLocationY : 100
				ScreenWidth : 600
				ScreenHeight : 400
			Perspective : Perspective : Gast on Cadeauwens
			Perspective : Perspective : Gast on Feest
			Perspective : Perspective : Gast on itnodiging
		Agent : UserRole : Host
			Mandatory : False
			Calculated : False
			Perspective : LocalPerspective : ContextScreen of Host
				ScreenLocationX : 100
				ScreenLocationY : 100
				ScreenWidth : 600
				ScreenHeight : 400
			Perspective : Perspective : Host on Cadeauwens
			Perspective : Perspective : Host on Feest
				Action : Changes : Changes
					ActionOnScreen : False
					ActionObjectView : AllProperties
					Role : ObjectRole : OR of Changes
						Object : Feest
					Role : SubjectRole : SR of Changes
						Subject : Host
				Action : Consults : Consults
					ActionOnScreen : False
					ActionObjectView : AllProperties
					Role : ObjectRole : OR of Consults
						Object : Feest
					Role : SubjectRole : SR of Consults
						Subject : Host
				Action : Creates : Creates
					ActionOnScreen : False
					ActionObjectView : AllProperties
					Role : ObjectRole : OR of Creates
						Object : Feest
					Role : SubjectRole : SR of Creates
						Subject : Host
				Action : Removes : Removes
					ActionOnScreen : False
					ActionObjectView : AllProperties
					Role : ObjectRole : OR of Removes
						Object : Feest
					Role : SubjectRole : SR of Removes
						Subject : Host
			Perspective : Perspective : Host on itnodiging
		Role : Role : itnodiging
			Mandatory : False
			NonFunctional : True
			Calculated : False
			FilledBy : Gast
			FilledBy : Host