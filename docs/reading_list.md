# Liste de lecture — Optimal Design en Pharmacométrie

Organisée en 3 niveaux : fondements NLME → théorie optimal design → applications récentes.
À lire dans cet ordre (les niveaux supérieurs supposent les inférieurs).

**Légende statut :** ✅ Lu | 🔄 En cours | ⬜ À lire

### Déjà lus (source : redaction.docx, 2026-03-07)
- ✅ Cours Leiden : Introduction to NONMEM, Introduction to Pharmacology, Modeling & Simulation
- ✅ Cours Uppsala : Optimal Design in Theory
- ✅ Bauer et al. (2021) — CPT:PSP — Tutorial for $DESIGN in NONMEM
- ✅ Mentré, Mallet & Baccar (1997) — Biometrika — FIM pour NLME
- ✅ Retout & Mentré (2003) — J Biopharm Stat — FIM avec FOCE
- ✅ Beal, Sheiner, Boeckmann, Bauer (2022) — NONMEM 7.5 Users Guide
- ✅ Mentré et al. (divers) — PAGE — Optimal design in PK and model selection

### En cours (source : redaction.docx)
- 🔄 Nyberg et al. (2012) — Comput Methods Programs Biomed — PopED
- 🔄 Retout, Comets, Samson, Mentré (2007) — Stat Med — Fedorov-Wynn algorithm + covariate testing
- 🔄 Ueckert et al. (2013) — J Pharmacokinet Pharmacodyn — Improved scaling of optimal design
- 🔄 Dodds et al. (2022) — Eur J Pharm Sci — Practical considerations for PopPK design

---

---

## Niveau 1 — Fondements NLME (prérequis théoriques)

### Livres disponibles localement
| Statut | Référence | Emplacement |
|--------|-----------|-------------|
| 🔄 | **Owen & Fiedler-Kelly (2014)** — *Introduction to Population PK/PD Analysis with NLMEM*, Wiley | `docs/books/PKPD Analysis with NLMEM/` |

### Statistiques et modèles mixtes
| Priorité | Statut | Référence | Pourquoi |
|----------|--------|-----------|---------|
| ★★★ | ⬜ | **Davidian & Giltinan (1995)** — *Nonlinear Models for Repeated Measurement Data*, Chapman & Hall | Référence fondatrice des NLME en pharmacométrie. Chapitres 2-4 sur les approximations (FO, FOCE, Laplace) et la FIM. |
| ★★★ | ⬜ | **Pinheiro & Bates (2000)** — *Mixed-Effects Models in S and S-Plus*, Springer | Base algorithmique. Comprendre le cadre mathématique des modèles mixtes. |
| ★★☆ | ⬜ | **Vonesh & Chinchilli (1997)** — *Linear and Nonlinear Models for the Analysis of Repeated Measurements* | Complémentaire à Davidian — focus sur la vraisemblance et les propriétés asymptotiques. |

### Pharmacométrie de base
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Beal & Sheiner (1982)** — *Estimating population kinetics*, CRC Crit Rev Biomed Eng | Article fondateur de NONMEM. Comprendre d'où vient l'approximation FO. |
| ★★☆ | **Sheiner & Beal (1985)** — *Pharmacokinetic parameter estimates from several least squares procedures*, J Pharmacokinet Biopharm | Comparaison des méthodes d'estimation — contexte historique essentiel. |
| ★★★ | **Holford (1999)** — *A size standard for pharmacokinetics*, Clin Pharmacokinet | Allometry et scaling — omniprésent dans les modèles PopPK modernes. |

---

## Niveau 2 — Théorie de l'optimal design

### Articles fondateurs
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Fedorov (1972)** — *Theory of Optimal Experiments*, Academic Press | Théorie mathématique originale des designs optimaux. Critères D, A, E. Difficile mais incontournable. |
| ★★★ | **Atkinson & Donev (1992)** — *Optimum Experimental Designs*, Oxford | Plus accessible que Fedorov. Chapitres 10-12 sur la D-optimalité et l'algorithme de Fedorov. |
| ★★☆ | **Kiefer (1959)** — *Optimum experimental designs*, J Royal Stat Soc B | Article original sur la D-optimalité. Court mais dense. |

### Adaptation aux NLME / pharmacométrie
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Mentré, Mallet & Baccar (1997)** — *Optimal design in random-effects regression models*, Biometrika | **Article clé** : formalise la FIM pour les NLME. Fondement mathématique de PFIM et $DESIGN. |
| ★★★ | **Retout & Mentré (2003)** — *Further developments of the Fisher information matrix in nonlinear mixed effects models*, J Biopharm Stat | Extension : approximation de la FIM avec FOCE. Directement lié aux calculs de NONMEM. |
| ★★★ | **Nyberg, Ueckert, Stroberg et al. (2012)** — *PopED: An extended, parallelized, nonlinear mixed effects models optimal design tool*, Comput Methods Programs Biomed | PopED — outil de référence pour comparer avec $DESIGN. Comprendre les différences algorithmiques. |
| ★★☆ | **Dodds, Hooker & Vicini (2005)** — *Robust population pharmacokinetic experiment design*, J Pharmacokinet Pharmacodyn | Robust design via la FIM espérée — base théorique de EOPTD=1 dans $DESIGN. |

### Critères d'optimalité
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Chaloner & Verdinelli (1995)** — *Bayesian experimental design: a review*, Stat Sci | Design bayésien — base de OFVTYPE=8 et du design robuste via $PRIOR. |
| ★★☆ | **Silvey (1980)** — *Optimal Design*, Chapman & Hall | Référence compacte sur les critères D, A, c, E et leurs propriétés géométriques. |

---

## Niveau 3 — Applications récentes et comparaisons d'outils

### Papiers directement liés à $DESIGN NONMEM
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Bauer, Guzy & Ng (2021)** — *Tutorial for $DESIGN in NONMEM*, CPT PSP | **Déjà lu** — référence principale du projet. |
| ★★★ | **Bauer (2011)** — *NONMEM users guide*, ICON | Manuel NONMEM 7.5 — déjà dans `docs/`. |
| ★★☆ | **Bazzoli, Retout & Mentré (2009)** — *Fisher information matrix for nonlinear mixed effects multiple response models*, Stat Med | Multi-réponses (pertinent pour exemple 4 : PK-PD). FIM multi-endpoints. |

### Comparaisons PFIM / PopED / $DESIGN
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Foracchia, Hooker, Vicini & Ruggeri (2004)** — *POPED, a software for optimal experiment design in population kinetics*, Comput Methods Programs Biomed | PopED original. Comprendre les différences avec $DESIGN. |
| ★★★ | **Retout, Duffull & Mentré (2001)** — *Development and implementation of the population Fisher information matrix for the evaluation of population pharmacokinetic designs*, Comput Methods Programs Biomed | PFIM — l'outil français, concurrent de $DESIGN. Mêmes équations, implémentation différente. |
| ★★☆ | **Nyberg et al. (2015)** — *Methods and software tools for design evaluation in population pharmacokinetics-pharmacodynamics studies*, Br J Clin Pharmacol | Review comparative PFIM vs PopED vs NONMEM $DESIGN. **À lire tôt** pour la mise en perspective. |

### SSE et validation
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Holford, Ma & Ploeger (2010)** — *Clinical trial simulation: a review*, Clin Pharmacol Ther | SSE (Stochastic Simulation Estimation) — méthode de validation des designs. Base de la phase 1 du projet. |
| ★★★ | **Vong, Bergstrand, Nyberg & Karlsson (2012)** — *Rapid sample size calculations for a defined likelihood ratio test-based power in mixed-effects models*, AAPS J | Calcul de puissance via FIM — lien direct avec la phase 2. |
| ★★☆ | **Mentre & Leroux (2017)** — *A simple illustration of the use of the PFIM function with pharmacokinetics* | Tutoriel PFIM pratique. Facile à lire, bon pour ancrer la théorie. |

### Spécifique anticorps monoclonaux (pertinent Frexalimab)
| Priorité | Référence | Pourquoi |
|----------|-----------|---------|
| ★★★ | **Dirks & Meibohm (2010)** — *Population pharmacokinetics of therapeutic monoclonal antibodies*, Clin Pharmacokinet | Référence sur les modèles PopPK d'anticorps — multi-compartiments, non-linéarité TMDD. |
| ★★☆ | **Dua et al. (2015)** — *A tutorial on target-mediated drug disposition (TMDD) models*, CPT PSP | TMDD — modèle utilisé dans les exemples 6-7, et potentiellement pertinent pour Frexalimab. |
| ★★☆ | **Gibiansky & Gibiansky (2009)** — *Target-mediated drug disposition model: relationships with indirect response models and application to population PK-PD analysis*, J Pharmacokinet Pharmacodyn | Approx. quasi-steady state TMDD — plus pratique que le TMDD complet. |

---

## Ordre de lecture recommandé pour le stage

**Phase 1 (mars–avril) — pendant l'évaluation des designs existants :**
1. Mentré, Mallet & Baccar (1997) — la FIM NLME théorique
2. Retout & Mentré (2003) — FOCE approximation dans la FIM
3. Nyberg et al. (2015) — comparaison des outils

**Phase 2 (mai–juin) — pendant l'optimal design :**
4. Dodds, Hooker & Vicini (2005) — robust design
5. Chaloner & Verdinelli (1995) — design bayésien
6. Foracchia et al. (2004) / Retout et al. (2001) — PFIM et PopED

**Fond (à lire quand besoin de théorie plus profonde) :**
- Davidian & Giltinan (1995) Chapitres 2-4
- Atkinson & Donev (1992) Chapitres 10-12

---

*Fichier généré le 2026-03-08. À mettre à jour au fur et à mesure des lectures.*
