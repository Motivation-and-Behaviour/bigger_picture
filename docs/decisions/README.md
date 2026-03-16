# Project Decision Register

This folder contains **Decision Records** documenting important decisions made during the project.

Decision records help maintain transparency, reproducibility, and shared understanding by recording **what decisions were made, why they were made, and what their consequences are**.

Each decision is stored as a separate markdown file following the [**ADR (Architecture Decision Record)**](https://adr.github.io) style.

---

## When decisions are recorded

A decision record is created when a choice:

- Ambiguity in the pre-registration creates confusion
- A violation of the pre-registration becomes necessary
- A harmonisation rule is adopted
- A decision may need to be revisited in the future

Routine or minor decisions are not recorded.

---

## File naming convention

Decision files follow the pattern:

```md
DR-XXXX-short-title.md
```

Example:

```md
DR-0001-data-schema-format.md
DR-0002-screen-time-harmonisation.md
DR-0003-missing-data-strategy.md
```

The numeric prefix preserves chronological ordering.

---

## Decision index

| ID      | Title | Status | Date |
| ------- | ----- | ------ | ---- |

When adding a new decision:

1. Create a new file using the template in `_template.md`
2. Assign the next available DR number
3. Add the decision to the table above

---

## Decision lifecycle

Decisions move through the following states:

- **Proposed** – under discussion
- **Accepted** – agreed and implemented
- **Superseded** – replaced by a later decision
- **Deprecated** – no longer relevant

Decisions should generally **not be edited after acceptance**.  
If a decision changes, create a new DR and reference the earlier one.

Example:

```md
Supersedes: ADR-0003
```

---

## Relationship to other documentation

Decision records complement other project documentation:

| Document          | Purpose                                                 |
| ----------------- | ------------------------------------------------------- |
| Harmonisation log | Records variable transformations and dataset processing |
| Analysis scripts  | Implement analytical decisions                          |
| Preregistration   | Describes planned analyses                              |
| Decision records  | Explain **why key choices were made**                   |

---

## Adding a new decision

1. Copy `_template.md`
2. Rename the file using the next ADR number
3. Complete the sections
4. Add the entry to the decision index table
5. Commit the change

---

## Related resources

- ADR concept: <https://adr.github.io/>
- Project documentation: `docs/`
