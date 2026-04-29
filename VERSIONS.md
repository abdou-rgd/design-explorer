# Versioning — DE$IGN EXPLORER

Noms de versions inspirés des chapitres du *Petit Prince* — Antoine de Saint-Exupéry.
Format technique : `MAJOR.MINOR.PATCH` (ex: `V4.12.3`)

> Les chapitres emblématiques sont réservés aux grandes mises à jour.
> Le dernier chapitre, *"La trace laissée"*, est réservé à la version finale.

---

## Versions publiées

| Version | Nom | Description |
|---------|-----|-------------|
| V1 | *Le dessin du serpent boa* | Parsers R initiaux — `read_ext()`, `read_shk()`, `get_rse()` |
| V2 | *La rencontre dans le désert* | Premiers vrais utilisateurs, app Shiny V1 |
| V3 | *Les baobabs* | Refactoring, restructuration codebase (`scripts/` → `R/`) |
| V4 | *La naissance de la rose* | App Shiny complète : 12 modules, multi-run, navigation sidebar, design robuste |
| V4.1.0 | — | Ex4/Ex5 intégrés, KPI bar supprimée, fix sprintf CSS%, fix unname(vapply) |
| V4.2.0 | — | Phase 2 quick wins : RSE 4 niveaux, labels CMT, percentiles TSTRAT, efficiency ratio, D-critère robuste, guides ex1/ex3 |
| V4.3.0 | — | Exemples Bauer 6 & 7, RSE colorés 4 tiers, export CSV temps optimaux, version sidebar, fix DT Run A |
| V4.3.1 | — | Review fixes : ligne 100% multi-run, accents titres, reactive robust_summary, Bootstrap 3, na.rm |
| V4.3.2 | — | Multi-run audit + refactor (PRs #28-#36) |
| V4.4.0 | — | Power Wald / NSN |
| V4.5.0 | — | TOST Equivalence + UX N total |
| V4.5.1 | — | Elementary design display fix |
| V4.5.2 | — | SSE Validation + Convergence + Upload fix |
| V5 | *Le d\u00e9part du Petit Prince* | Refonte V5 : Home page, kill drawer, English labels, rename DE$IGN EXPLORER |

---

## Version actuelle

**V5.8** — *Terre des hommes* — Times-tab redesign, codebase review fixes, and reproducible test hygiene

---

## Chapitres disponibles (pool)

| Chapitre | Statut |
|----------|--------|
| Le dessin du serpent boa | V1 ✓ |
| La rencontre dans le désert | V2 ✓ |
| L'astéroïde du Petit Prince | disponible |
| Le regard des grandes personnes | disponible |
| Les baobabs | V3 ✓ |
| Les couchers de soleil | disponible |
| La rose et la peur de la perdre | disponible |
| La naissance de la rose | V4 ✓ |
| Le d\u00e9part du Petit Prince | V5 \u2713 |
| Le roi | disponible |
| Le vaniteux | disponible |
| Le buveur | disponible |
| Le businessman | réservé (grande MAJ) |
| L'allumeur de réverbères | disponible |
| Le géographe | disponible |
| La Terre | réservé (grande MAJ) |
| Le serpent | disponible |
| La fleur du désert | disponible |
| La montagne et l'écho | disponible |
| Le jardin de roses | disponible |
| Le renard | réservé (grande MAJ) |
| L'aiguilleur | disponible |
| Le marchand de pilules | disponible |
| Le puits | disponible |
| Les étoiles | réservé (grande MAJ) |
| La morsure du serpent | disponible |
| La trace laissée | reserve (version FINALE) |

---

## Conventions

- Le **nom poétique** est pour nous — culture interne, communication entre nous.
- Le **numéro technique** (`V4.1.0`) est pour le suivi des releases.
- Les chapitres **réservés** attendent une MAJ significative qui leur correspond.
- Le nom est choisi en fonction de ce que la version apporte, pas dans l'ordre du livre.
- Les **versions mineures** (V4.1.0, V4.2.0...) n'ont pas de nom poetique — seules les versions majeures (V1, V2, V3, V4...) en recoivent un.
