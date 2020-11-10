# Analýzy otevřených dat pro COVID-19 v České republice


## Vyhodnocení předpovědi

Tečky reprezentují datové údaje, ze kterých vychází předpověď. Křížky
jsou pak údaje z následujících dnů, aby bylo možné sledovat kvalitu předpovědi.

Upozornění: Předpověď exponenciálních trendů je mimořádně obtížná, zejména
při nedostatku kvalitních vstupních dat. Nedoporučuji dělat z následujících
předpovědí žádné významné závěry.

![Poslední předpověď](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/covid_forecast_eval_latest.png)

### Vyhodnocení předpovědi v čase

![Poslední předpověď](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/covid_fcast_anim_20201110.gif)

## Regionální přehledy aktuální situace

[Viz samostatná stránka](region.md)


## Lokální hotspoty

Velikost bodu reprezentuje podíl infikovaných obyvatel v dané obci. Barva bodu
odpovídá velikosti přírůstku případů za posledních 7 dnů (v poměru k počtu
zdravých obyvatel).

![Lokální hotspoty](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/covid_hotspots_latest.png)


## Dynamika šíření nákazy

Graf ilustruje poměr počtu nových případů mezi dny D+7 a D. Počet nových případů
je reprezentovaný váženým součtem zachycených případů v několika po sobě jdoucích
dnech, aby se potlačil efekt různých dnů v týdnu.

![Dynamika šíření](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/spread_factor.png)

Následující graf ukazuje vztah mezi relativním počtem aktivních případů
(vztaženo k počtu obyvatel spadajících pod danou obec s rozšířenou působností)
a relativním počtem nových případů za uplynulý týden (vztaženo k počtu doposud
zdravých obyvatel).

Z dosavadního vývoje lze odhadovat existenci pozitivní zpětné vazby, která má
za následek exponenciální průběh epidemie.

![Dynamika šíření](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/covid_dyn_latest.png)


## Efekt dnů v týdnu

Distribuce nových případů v rámci týdne není rovnoměrná, zejména kvůli provozní
době a kapacitě odběrových míst. Graf ukazuje, jaký podíl nových případů typicky
připadá na daný den v týdnu.

![Efekt dnů v týdnu](https://raw.githubusercontent.com/bbobcik/covid-cz/master/outputs/weekday_effect.png)


## Zdroje dat

* Komenda M., Karolyi M., Bulhart V., Žofka J., Brauner T., Hak J., Jarkovský J.,
  Mužík J., Blaha M., Kubát J., Klimeš D., Langhammer P., Daňková Š., Májek O.,
  Bartůňková M., Dušek L.
  *COVID‑19: Přehled aktuální situace v ČR. Onemocnění aktuálně [online]*.
  Praha: Ministerstvo zdravotnictví ČR, 2020 [cit. 22.10.2020]. Dostupné z:
  https://onemocneni-aktualne.mzcr.cz/covid-19.
