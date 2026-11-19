The CPMpy model has one decision variable per surface cell. The function
`build_model(givens)` accepts the clue string for any instance in the dataset.

From this problem's directory, use Python 3.12 in a virtual environment.

```sh
python -m pip install -r assets/requirements.txt
python models/cube_sudoku.py
python models/verify_examples.py
```

The first script solves the embedded Easy example with OR-Tools CP-SAT and
prints its solution as JSON. The verifier checks the model's cell order and
groups against `data/topology.json`, then solves each of the five examples and
compares the answers to the released solutions. A second solve excludes each
known solution and must return `UNSATISFIABLE`, proving uniqueness for that
instance.

The Python files are available under [Apache 2.0](../assets/Apache-2.0.txt).
The sample data and topology retain their CC BY 4.0 licence.
