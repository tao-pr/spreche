# Spreche

A weekend project helping myself learning basic German.

### Build and Run

Simply use sbt

```bash
  sbt compile
  sbt spreche/run
```

### Supported sentence structures

The project is continuously developed and following structures are supported so far.

<b>Nominativ</b>

```
Sagen Sie > er ist mein laptop

Output > er ist mein Laptop

```

<b>Akkusativ</b>

```
Sagen Sie > du nehme die tram

Output > du nimmst die Tram

---

Sagen Sie > du senden mein lampe

Output > du sendest meine Lampe

```

<b>Dativ</b>

```
Sagen Sie > meine Uhr steht auf der tisch

Output > meine Uhr steht auf dem Tisch

---

Sagen Sie > wir ist in das zimmer

Output > wir sind im Zimmer

```

<b>Mixed Dativ with Akkusativ</b>

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