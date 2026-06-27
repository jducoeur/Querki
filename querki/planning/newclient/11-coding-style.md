# Coding Style for the New Client (Scala 3)

The new client is the **first Scala 3 code in the codebase**, so we set style guidelines up front.
The baseline is the community **"Common Scala Style Recommendation"**
([contributors.scala-lang.org thread](https://contributors.scala-lang.org/t/towards-a-common-scala-style-recommendation/7383)),
with **owner nuances** about how aggressively to go braceless.

> **Purpose of this doc:** these guidelines are substantially **for the assistant's benefit** — so
> that generated/edited code looks about right *before* scalafmt runs. scalafmt is deliberately
> *not* configured to rewrite brace choices (see below), so the human-written intent is what ships.
> **Guiding principle: when in doubt, use braces.**

## The baseline: "use fewer braces, not no braces"
The recommendation deliberately splits the difference between full-braces and fully-braceless
(significant-indentation) Scala 3. Its concrete points:

- **Braces around long scopes that aren't terminated by a keyword.** A scope is "long" if it
  contains **blank lines** (not already inside a nested construct). Short scopes go braceless.
- **No braces needed when a scope is naturally closed by a keyword** — `else`, `do`, `yield`,
  `case`, `catch` — since the keyword already delimits the block.
- **Use blank lines to reflect logical structure**, compensating for the vertical whitespace that
  closing braces used to provide.
- **Add braces wherever they aid understanding** — flexibility for edge cases is fine.
- **Prefer the modern control syntax**: `if … then … else`, `while … do`, `for … do`/`yield` over
  the parenthesized forms.
- **Class/def bodies and call-site blocks**: `:`-plus-indent *or* braces, as you prefer — the
  recommendation gives no ruling there.
- **End markers** (`end foo`): the recommendation deliberately stays neutral / doesn't push them.
- It gives **no** guidance on indentation width, naming, etc. (those follow existing repo norms).

## The owner's nuance (this is the operative rule)
> **Braceless style only for particularly short clauses — typically five or fewer lines. Beyond
> that, prefer braces.**

So in practice:
- **≤ ~5 lines, no blank lines** → braceless / significant-indentation is good (matches the
  recommendation's "short scope" case).
- **More than ~5 lines, or any blank line in the scope** → use braces. This is stricter/more concrete
  than the recommendation's "contains blank lines" trigger: the owner wants the line-count threshold
  as the primary guide, with the blank-line rule reinforcing it.
- Treat "5" as a guideline, not a hard lint — judgment for readability still applies, and braces are
  always acceptable "where they aid understanding." **When in doubt, use braces.**

Two further owner rules:
- **`end` markers: actively avoid them.** (The owner reads `end foo` as Martin pining for the
  Pascal days.) Don't emit `end` markers; if a braced/indented block is long enough that an `end`
  marker feels tempting, that's a signal to use **braces** instead.
- **The `:`-indent (colon) body syntax: only for very short bodies with no blank lines.** For
  anything longer — or anything with a blank line — use **braces** for the body too. (This applies
  the ≤5-line nuance to class/def/object/call bodies, where the base recommendation stays neutral.)

## Adopted vs. left-to-taste (quick reference for the new client)
| Topic | Guideline |
|---|---|
| Short clause (≤5 lines, no blanks) | Braceless / indentation OK |
| Longer clause (>5 lines or blank lines) | **Braces** |
| Scope closed by `else`/`do`/`yield`/`case`/`catch` | Braceless fine regardless |
| Control syntax | Prefer `if/then/else`, `while/do`, `for/do`/`yield` |
| Blank lines | Use them to show logical structure |
| `end` markers | **Avoid.** Don't emit them; reach for braces instead if a block feels long enough to want one |
| `:`-indent vs braces for class/def/call bodies | `:` **only** for very short bodies with no blank lines; otherwise **braces**. When in doubt, braces |
| `given`/`using` | Standard Scala 3 (replaces `implicit` in new code) |

## Carry-over from existing repo conventions (don't re-litigate)
These already hold across the repo and continue to apply to the new client:
- **`maxColumn = 120`**, **sorted imports**, **`-Xfatal-warnings`** + unused-import checking on
  (warnings break the build — see CLAUDE.md / `.scalafmt.conf`).
- Import `querki.globals._`-style aggregations rather than grab-bag imports (the new client will have
  its own `globals` analogue).

## scalafmt implications (build-setup note)
**Decided:** the new project gets its **own `.scalafmt.conf`** (the existing one pins scalafmt
**2.7.5**, which predates good Scala 3 support, and the Scala 2 server should keep formatting as-is —
an isolation-friendly split, doc 04). It uses a **newer scalafmt** with **`runner.dialect = scala3`**.

**Decided:** do **not** enable `rewrite.scala3.removeOptionalBraces`. It's all-or-nothing and would
strip braces the owner subjectively wants — and the whole point is that brace choice stays a
deliberate human decision (this doc exists so the assistant gets it right *before* scalafmt). So
scalafmt **won't touch brace/`:` choices at all**.

- scalafmt **will** handle the mechanical bits: import sorting, `maxColumn = 120`, general layout,
  and (optionally) `rewrite.scala3.convertToNewSyntax` for the modern control syntax — confirm that
  one when scaffolding; it's compatible with the nuance since it converts `if (...)`→`if ... then`
  without removing block braces.
- scalafmt **will not** (and cannot) express the "braces beyond ~5 lines" / "no `end` markers" /
  "`:` only for tiny bodies" rules — those are **human conventions** captured here, enforced by the
  assistant and review, not the formatter.
- Confirm the exact settings when we scaffold the project.

## Where this is recorded
This is a durable coding preference; also captured in memory as a `feedback` entry so it applies to
future Scala 3 work in this codebase, not just this planning round.
