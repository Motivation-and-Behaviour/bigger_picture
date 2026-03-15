# Decision Record Template

This template is used to document important decisions made during the project.

Decision records help ensure transparency, reproducibility, and shared understanding across the team. Each decision should explain **what was decided, why it was decided, and what the implications are**.

## When to create a decision record

Create a record when:

- Ambiguity in the pre-registration creates confusion
- A violation of the pre-registration becomes necessary
- A harmonisation rule is adopted
- A decision may need to be revisited in the future

Do **not** create records for minor or routine decisions.

## File naming convention

Each decision should be stored as a separate markdown file:

```md
docs/decisions/DR-XXXX-short-title.md
```

Example:

```md
DR-0001-data-schema-format.md
DR-0002-screen-time-harmonisation.md
DR-0003-missing-data-strategy.md
```

Use sequential numbering to preserve chronological order.

## Updating decisions

Decisions should generally **not be edited after acceptance**. If a decision changes, create a new record that:

- references the earlier decision
- explains why the change occurred

Example:

```md
Supersedes: DR-0003
```

---

## Template

```md
    
    # DR-XXXX: Short descriptive title

    **Status:** Proposed | Accepted | Superseded | Deprecated  
    **Date:** YYYY-MM-DD  
    **Decision makers:** Names or team  
    **Related issues:** Links to issues, PRs, or discussions

    ---

    ## Context

    Describe the situation that required a decision.

    Include enough detail so that someone unfamiliar with the discussion can understand:

    - the problem
    - relevant constraints
    - alternatives considered

    Example topics may include:

    - data harmonisation challenges
    - methodological choices
    - software or infrastructure decisions
    - governance or data management considerations

    ---

    ## Decision

    Clearly state the decision that was made.

    This section should be concise and unambiguous.

    ---

    ## Rationale

    Explain **why this decision was chosen** over alternatives.

    Possible considerations include:

    - methodological validity
    - reproducibility
    - compatibility with existing datasets
    - computational feasibility
    - alignment with project goals

    ---

    ## Alternatives considered

    Briefly describe other options that were considered and why they were not chosen.

    Example:

    - Option A – rejected because …
    - Option B – rejected because …

    ---

    ## Consequences

    Describe the implications of this decision.

    These may include:

    ### Positive consequences

    - improves consistency across datasets
    - simplifies downstream analysis

    ### Negative consequences or trade-offs

    - increases preprocessing complexity
    - requires additional documentation

    ---

    ## Implementation

    Describe how the decision is implemented in practice.

    Examples:

    - scripts or functions implementing the decision
    - harmonisation rules
    - data transformations

    Example links:

    ```md
    R/harmonise_screen_time.R
    harmonisation_log.md
    targets pipeline step: harmonise_dataset()
    ```

    ---

    ## Related decisions

    List related decision records if applicable.

    Example:

    ```md
    DR-0002: Data schema structure
    DR-0005: Missing data strategy
    ```
```
