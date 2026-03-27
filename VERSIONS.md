# Versioning — $DESIGN Explorer

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

---

## Version actuelle

**V4.1.0** — *La naissance de la rose* 🌹

## Prochaine version

**V4.2.0** (PRs #14–#19 en attente de merge) — Phase 2 Quick Wins

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
| Le départ du Petit Prince | disponible |
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
