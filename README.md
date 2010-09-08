Client:
-------

Abhängigkeiten:
========================
Alle Abhängigkeiten können unter Ubuntu aus den Paketquellen installiert werden.
Dazu ist es notwendig folgende Pakete zu installieren:

ghc6
libghc6-network-dev
libghc6-parsec2-dev

Möglicher Aufruf:

`sudo apt-get install ghc6 libghc6-network-dev libghc6-parsec2-dev`


Erstellen des Programms
========================
Zum Erstellen des Programms reicht der Aufruf `make`, alternativ `make build`.

Starten des Programms
=====================

Das Programm kann gestartet werden, indem "./wuerfeln" aufgerufen wird, alternativ über den Aufruf `make game`.
Ein Aufruf ohne Parameter führt dazu, dass das Programm versucht sich zum Server unter wettbewerb.linux-magazin.de zu verbinden um dort an einer Partie teilzunehmen.
Nach dem Spielen einer Partie, oder wenn keine Verbindung zu stande kam, beendet sich das Programm.


Parameter:
==========
Das Programm versteht folgende Parameter:

* s - Adresse der Servers bspw. 127.0.0.1 für ein lokales Spiel
* n - Name des Spielers mit dem sich der Client beim Server authentifiziert
* p - Portnummer zu der der Client die Verbindung aufbaut

Wichtig:

Parameter und deren Werten müssen ohne Leerzeichen angegeben werden.
Beispielsweise:
    ./wuerfeln -s127.0.0.1 -nNeuerName


Server:
-------

Im Umfang der Software ist auch eine Serverimplementierung enthalten, die über `make server` gestartet werden kann.

Die Abhängigkeiten des Servers sind die selben des Clients.

