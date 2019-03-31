# Spreche

A weekend project helping myself learning basic German.

### Build and Run

Simply use sbt

```bash
  sbt compile
  sbt spreche/run
```

### Sentence construction

`Spreche` comes with a simple language model which constructs the tokens the user puts in as so:

```
Sagen Sie > 
Sagen Sie > ich habe viele block
Output = ich habe die Blöcke

Sagen Sie > 
Sagen Sie > ein uhr ist neben der hamd
Output = eine Uhr ist neben dem Hamd

Sagen Sie >
Sagen Sie > wir ist in das zimmer
Output = wir sind im Zimmer
```