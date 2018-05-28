# PP
Clanovi tima: Marija Filipovic 482/2017, Djordje Stanojevic 224/2015
Tema: Numericki algoritmi

github link: https://github.com/djordjestanojevic/PP

Da bi projekat radio potreban je cabal base >=4.9 && <4.10 i ghc bar version 8.0.2.

Projekat se pokrece pozicioniranjem u terminalu u unzipovan direktorijum i potom nizom naredbi:
	$cabal configure
	$cabal build
	$cabal install
	$cabal repl
Cime se automatski pokrece ghci i aktivan je modul "Regula_falsi" ukoliko zelimo da ucitamo neki drugi modul to radimo sa :add
Ime_modula

Implementacija numerickih algoritama:
  Lagranzov interpolacioni polinom
  Njutnov interpolacioni polinom sa podeljenim razlikama(unapred,unazad)
  Njutnov interpolacioni polinom sa konacnim razlikama(prvi,drugi)
  Gausov interpolacioni polinom sa konacnim razlikama(prvi,drugi)
  Beselov interpolacioni polinom
  Inverzna interpolaciona 
  Numericko diferenciranje
  Kvadraturne formule pravougaonika,trapeza i Simpsona
  Lezandrovi polinomi
  Cebisevljevi polinomi
  Metode resavanja nelinearnih jednacina 
	polovljenje intervala
	regula falsi
	metoda secice
	Njutnova metoda tangente sa modifikacijama
	iterativna sa modifikacijama
