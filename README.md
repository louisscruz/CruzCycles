# Cruz Cycles

This project detects chains of synonyms that terminate on an antonym of the source word. For instance, the following cycle begins with a source ("thoughtful") and ends an antonym of the source ("heedless"), connected through a chain of synonyms:

```
thoughtful, serious, sincere, artless, careless, heedless
```

The relevant code is located in [this package][package], and the tests are located [here][tests].

Thanks has to be given to Princeton University's [WordNet][word-net], the thesaurus on which this project relies.

[package]: ./src/main/scala-2.12/CruzCycles/package.scala
[tests]: ./src/test/scala-2.12/com/test/CruzCyclesTest.scala
[word-net]: https://wordnet.princeton.edu/