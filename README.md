[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/SsQ6IvG7)

# Shogi Game

This project's dependencies are managed using [`poetry`](https://github.com/python-poetry/poetry).

```bash
# Installs dependencies (Poetry)
poetry install
```

## Linting & Formatting

Before pushing a commit, you may want to run the [`ruff`](https://github.com/astral-sh/ruff) linter and formatter.

```bash
# Runs linter (Ruff)
poetry run ruff check

# Runs formatter (Ruff)
poetry run ruff format
```

## Running Tests

The [`pytest`](https://github.com/pytest-dev/pytest) framework is used to write and run tests. These tests may be executed by running:

```bash
# Executes tests (Pytest)
poetry run pytest
```
