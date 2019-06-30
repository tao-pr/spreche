# Spreche

A weekend project helping myself learning basic German.

## Build and Run

Simply use sbt

```bash
  sbt compile
  sbt spreche/run
```

## Supported sentence structures

The project is continuously developed and following structures are supported so far.

### Nominativ

```
Sagen Sie > er ist mein laptop

Output > er ist mein Laptop

```

### Akkusativ

```
Sagen Sie > du nehme die tram

Output > du nimmst die Tram

---

Sagen Sie > du senden mein lampe

Output > du sendest meine Lampe

```

### Dativ

```
Sagen Sie > meine Uhr steht auf der tisch

Output > meine Uhr steht auf dem Tisch

---

Sagen Sie > wir ist in das zimmer

Output > wir sind im Zimmer

```

### Mixed Dativ with Akkusativ

```
Sagen Sie > du bringst mich meinen bild

Output > du bringst mir mein Bild

---

Sagen Sie > du gibt mir dein lampe

Output > du gibst mir deine Lampe

---

Sagen Sie > wir sitzen mit mein auto in das kino

Output > wir sitzen mit meinem Auto im Kino


---

Sagen Sie > ihr gibt uns kein schuh

Output > ihr gebt uns keinen Schuh

```

### Modal verb

```
Sagen Sie > ich will einen kugelschreiber kauft

Output > ich will einen Kugelschreiber kaufen

---

Sagen Sie > wir muss er einen becher bringt

Output > wir müssen ihm ein Becher bringen

```

### Negation

```
Sagen Sie > wir mag nicht die musik

Output > wir mögen die Musik nicht

---

Sagen Sie > ein freund soll in das cafe kommen nicht

Output > ein Freund soll ins Cafe nicht kommen


```

### Time

```
Sagen Sie > das kind darf zu haus gehen um 15:30 uhr

Output > um halbsechszehn Uhr darf das Kind zu ein Haus einer Uhr gehen

---

Sagen Sie > wir will mit unser auto zu das kino fahren am montag

Output > Montag wollen wir mit unserem Auto zum Kino fahren

```

### Perfekt tense

```
Sagen Sie > mein freund und ich habt zu das groß kino komme

Output > mein Freund und ich sind zum großen Kino gekommen

---

Sagen Sie > er hat kaufen dir ein neu buch

Output > er hat dir ein neues Buch gekauft
```


