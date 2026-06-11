# 🇵🇱 Polska A i B, czyli analiza dualizmu społeczno-gospodarczego

Cześć! Projekt, który tu widzisz, powstał w ramach przedmiotu **Analiza Wielwymiarowa** na Wydziale Zarządzania Uniwersytetu Gdańskiego. 

**Autorzy:** [Daniel Faltynowski](https://github.com/DanielFaltynowski) & [Dawid Kalinowski](https://github.com/dawid-kalinowski)

### 🎯 O co w tym chodzi?
Głównym celem naszej analizy było sprawdzenie, czy historyczny podział na „Polskę A” i „Polskę B” wciąż ma odzwierciedlenie w liczbach. Chcieliśmy dokopać się do ukrytych struktur w danych i sprawdzić, pod jakimi konkretnie względami polskie województwa różnią się od siebie najbardziej.

### 📊 Skąd mieliśmy dane?
Wszystkie statystyki wyciągnęliśmy z **Banku Danych Lokalnych GUS**. Żeby analiza miała sens i była sprawiedliwa, wszystkie surowe liczby przeliczyliśmy *per capita* (na mieszkańca) lub na $km^2$. Gdybyśmy tego nie zrobili, w każdym rankingu bezkonkurencyjnie wygrywałyby największe województwa, niezależnie od realnego poziomu życia czy rozwoju.

Łącznie zebraliśmy aż 56 zmiennych podzielonych na kilkanaście obszarów tematycznych, od demografii i gospodarki, przez edukację i ochronę zdrowia, aż po turystykę.

### 🛠️ Jakich metod użyliśmy? (I po co?)
Do przeanalizowania tego zbioru w R wykorzystaliśmy cztery potężne narzędzia statystyczne:

* **Analiza korelacji kanonicznych**: żeby sprawdzić, czy i jak mocno wskaźniki czysto gospodarcze (jak PKB czy zarobki) wpływają na realne inwestycje w regionach.
* **Analiza głównych składowych (PCA)**: pomogła nam „ścisnąć” dziesiątki różnych zmiennych w kilka najważniejszych makro-wskaźników, żeby nie pogubić się w gąszczu danych.
* **Porządkowanie liniowe**: dzięki niemu stworzyliśmy sprawiedliwy, syntetyczny ranking województw od „najlepszego” do „najgorszego”.
* **Analiza skupień**: pozwoliła nam pogrupować województwa w naturalne „klastry” i zobaczyć na mapie, czy te grupy faktycznie układają się w tradycyjny podział wschód-zachód.

---
*Kod źródłowy, wykresy i pełną interpretację wyników znajdziesz w plikach repozytorium. Zapraszamy do przeglądania!*

|GRUPA | ZMIENNA | OPIS |
| :--- | :---: | :--- |
| DEMOGRAFIA | x01 | gęstość zaludnienia (os / km2) - 2021 SP |
| DEMOGRAFIA | x02 | wskaźnik urbanizacji 2021 - spis powszechny - ile osób % mieszka w miastach |
| DEMOGRAFIA | x03 | emigranci - 2021 sp - odsetek emigrantów w % względem ludności województwa - czasowy pobyt za granicą |
| DEMOGRAFIA | x04 | LUDNOŚĆ PRZYBYŁA DO MIEJSCOWOŚCI AKTUALNEGO ZAMIESZKANIA W LATACH 2011-2021 Z INNEGO MIEJSCA W KRAJU |
| DEMOGRAFIA | x05 | Przyrost naturalny - 2024 w % rok do roku |
| DEMOGRAFIA | x06 | saldo migracji stałej w % rok do roku - 2024 |
| EDUKACJA | x07 | dzieci w przedszkolach i innych formach wychowania przedszkolnego na 1 tys. dzieci w wieku 3-5 lat 2024 |
| EDUKACJA | x08 | liczba studentów przypadających na jednego nauczyciela akademickiego - 2024 |
| EDUKACJA | x09 | studenci uczelni na 1,000 ludności - 2024 |
| EDUKACJA | x10 | zdawalność egzaminów maturalnych - 2024 |
| GOSPODARKA | x11 | pkb na osobę w 2023 w 1000 zł |
| GOSPODARKA | x12 | podmioty wpisane do rejestru REGON na 1 tys. ludności – 2024 |
| GOSPODARKA | x13 | dochody budżetów województw na 1 mieszkańca w roku 2024 w dziesiątkach złotych |
| GOSPODARKA | x14 | jednostki nowo zarejestrowane w rejestrze REGON w roku 2024 na 10 tys. ludności |
| GOSPODARKA | x15 | Przeciętne miesięczne wynagrodzenie brutto w 2024 w setkach zł |
| INFRASTRUKTURA TRANSPORTOWA | x16 | liczba rowerów publicznych na 100,000 ludności 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x17 | drogi dla rowerów na 1,000 km2 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x18 | kilometry dróg publicznych ogółem na 100 km^2 - 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x19 | kilometry dróg publicznych o nawierzchni twardej na 100 km^2 - 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x20 | (przystanki autobusowe (z trolejbusowymi) i tramwajowe, przystanki wspólne dla tramwajów i autobusów) na 100km^2 w 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x21 | długość linii komunikacji miejskiej na 100,000 mieszkańców 2024 w km |
| INFRASTRUKTURA TRANSPORTOWA | x22 | przewozy pasażerskie na 1 mieszkańca 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x23 | LINIE REGULARNE KOMUNIKACJI AUTOBUSOWEJ w km na 100km^2 w 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x24 | linie kolejowe ogółem na 1000 km2 w 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x25 | samochody osobowe na 100 mieszkańców 2024 |
| INFRASTRUKTURA TRANSPORTOWA | x26 | wypadki drogowe na 100,000 mieszkańców 2024 |
| INWESTYCJE | x27 | nakłady inwestycyjne w przedsiębiorstwach na 1 mieszkańca w 2024 w setkach zł |
| INWESTYCJE | x28 | WYDATKI BUDŻETÓW GMIN I MIAST NA PRAWACH POWIATU na 1 mieszkańca w 2024 w setkach zł |
| INWESTYCJE | x29 | Wydatki budżetów gmin i miast na prawach powiatu na kulturę i ochronę dziedzictwa narodowego na 1 mieszkańca w 2024 w dziesiątkach złotych |
| INWESTYCJE | x30 | Wydatki budżetów gmin i miast na prawach powiatu na oświatę na 1 mieszkańca w 2024 w setkach zł |
| INWESTYCJE | x31 | Wydatki budżetów gmin i miast na prawach powiatu na ochronę powietrza atmosferycznego i klimatu na 1 mieszkańca w 2024 w setkach zł |
| INWESTYCJE | x32 | wydatki inwestycyjne budżetów gmin i miast na prawach powiatu na 1 mieszkańca w 2024 w setkach zł |
| KULTURA | x33 | biblioteki publiczne na 100,000 ludności w 2024 |
| KULTURA | x34 | pozycje księgozbioru bibliotek publicznych na 100 ludności |
| KULTURA | x35 | średnia liczba godzin otwarcia biblioteki publicznej po godz. 16.00 w 2024 / powiększone 10-krotnie |
| KULTURA | x36 | zajęcia prowadzone przez teatry i instytucje muzyczne, muzea oraz centra, domy, ośrodki kultury, kluby i świetlice na 10,000 ludności w 2024 |
| KULTURA | x37 | uczestnicy imprez (wydarzeń kulturalnych) organizowanych przez teatry i instytucje muzyczne, muzea, galerie sztuki, kina oraz centra, domy kultury, kluby i świetlice na 1,000 ludności w 2024 |
| OCHRONA ZDROWIA | x38 | Lekarze pracujący bezpośrednio z pacjentem według województw w przeliczeniu na 10 tys. mieszkańców – stan w dniu 31 grudnia 2023 |
| OCHRONA ZDROWIA | x39 | Lekarze dentyści pracujący bezpośrednio z pacjentem według województw – na 10 tys. mieszkańców – stan 31 grudnia 2023 |
| OCHRONA ZDROWIA | x40 | Pielęgniarki pracujące bezpośrednio z pacjentem według województw na 10 tys. mieszkańców – 31 grudnia 2023 |
| OCHRONA ZDROWIA | x41 | Położne na 10,000 kobiet 31 grudnia 2023 |
| OCHRONA ZDROWIA | x42 | Łóżka na oddziałach kardiologicznych na 100 tys. ludności |
| STAN MATERIALNY | x43 | Stopa promil bezrobocia w grudniu 2024 |
| STAN MATERIALNY | x44 | BENEFICJENCI ŚRODOWISKOWEJ POMOCY SPOŁECZNEJ w promilach |
| TURYSTYKA | x45 | miejsca noclegowe na 1000 ludności w 2024 |
| TURYSTYKA | x46 | udzielone noclegi ogółem styczeń-grudzień na 10 ludności w 2024 |
| WARUNKI BYTOWE | x47 | Liczba mieszkań na 100 mieszkańców w 2024 r |
| WARUNKI BYTOWE | x48 | Odsetek ludności w obiektach zbiorowego zakwaterowania 2021 / powiększone 100-krotnie |
| WARUNKI BYTOWE | x49 | liczba miejsc w domach studenckich w stosunku do liczby studentów 2024 - jaki odsetek studentów domy studenckie są w stanie pomieścić |
| WARUNKI BYTOWE | x50 | studenci korzystający z domów studenckich w % ogółu studentów 2024 |
| WARUNKI BYTOWE | x51 | zasoby mieszkaniowe gmin komunalne 2024 na 10,000 mieszkańców |
| ZASOBY LUDZKIE | x52 | WSPÓŁCZYNNIK AKTYWNOŚCI ZAWODOWEJ OSÓB W WIEKU 15-89 LAT w 4 kwartale 2024 |
| ZASOBY LUDZKIE | x53 | odsetek ludności w wieku produkcyjnym - 2021 sp |
| ZASOBY LUDZKIE | x54 | odsetek ludności z wykształceniem wyższym - 2021 sp |
| ZASOBY LUDZKIE | x55 | Współczynnik obciążenia demograficznego (liczba osób w wieku nieprodukcyjnym na 100 osób w wieku produkcyjnym) - 2024 |
| ZASOBY LUDZKIE | x56 | Współczynnik obciążenia demograficznego (liczba osób w wieku nieprodukcyjnym na 100 osób w wieku produkcyjnym) - 2024 |