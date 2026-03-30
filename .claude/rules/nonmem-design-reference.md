# NONMEM $DESIGN — Référence

## Offsets `docs/nonmem/manuel_nonmem.txt` (numéros de ligne exacts)

| Section | Contenu | Ligne début | Ligne fin approx. |
|---------|---------|-------------|-------------------|
| I.34 | Introduction aux méthodes EM et Monte Carlo | 4322 | 4326 |
| I.35 | ITS (Iterative Two Stage) | 4327 | 4335 |
| I.36 | IMP (Importance Sampling EM) | 4336 | 4448 |
| I.37 | IMPMAP (IMP + MAP) | 4449 | 4458 |
| I.38 | SAEM | 4459 | 4605 |
| I.39 | BAYES (MCMC) | 4606 | 4685 |
| I.40 | NUTS (No U-Turn Sampler, NM74) | 4686 | 4956 |
| I.41 | $PRIOR NWPRI — structure des priors | 4957 | 5177 |
| I.43 | Notes générales EM + options $EST | 5190 | 5275 |
| I.44 | MU-Referencing | 5276 | 5477 |
| I.56 | $COV — options supplémentaires | 5922 | 6112 |
| I.61 | Format du fichier .res (rapport) | 6672 | 6748 |
| I.62 | Format du fichier .ext (raw output) | 6765 | 6859 |
| I.63 | Fichiers supplémentaires (.phi, .ets…) | 6860 | 7043 |
| I.67 | $RCOV / $RCOVI (design séquentiel) | 7317 | 7428 |
| I.72 | $DESIGN — évaluation et optimisation | 7521 | 8048 |
| I.73 | Parallel Computing | 8049 | 9099 |

---

## Structure des fichiers $DESIGN NONMEM

### Fichier `.ext`
- En-tête : `TABLE NO. 1: First Order (Evaluation): D-OPTIMALITY: ...`
- Colonnes : `ITERATION`, `THETA1..n`, `OMEGA(x,x)..`, `SIGMA(x,x)..`, `OBJ`
- Dans le contexte $DESIGN, `OBJ` = critère d'optimalité (ex: `-log(det(FIM))`)
- Lignes spéciales :

| Itération | Contenu |
|-----------|----------|
| `-1000000000` | Résultat final (paramètres + OBJ) |
| `-1000000001` | Erreurs standard (SE) |
| `-1000000002` | Valeurs propres matrice de corrélation |
| `-1000000003` | Condition number, min/max eigenvalues |
| `-1000000004` | OMEGA/SIGMA en format SD/corrélation |
| `-1000000005` | SE des éléments SD/corrélation |
| `-1000000006` | Indicateur paramètre fixé (0/1) |
| `-1000000007` | Code de terminaison |
| `-1000000008` | Dérivée partielle log-vraisemblance |

### Fichier `.clt`
- Forme triangulaire inférieure de la matrice variance-covariance
- Ordre TOSL (Thetas, Omegas, Sigmas, Lower-triangular)
- En contexte $DESIGN : contient la **FIM** (= `.coi` en triangulaire)

### Fichier `.coi`
- **Fisher Information Matrix complète** (matrice inverse de covariance)
- Format nommé avec headers (THETA1, THETA2, OMEGA(1,1), SIGMA(1,1)...)
- Utilisé par `$RCOVI` pour le design séquentiel

### Fichier `.res`
- Rapport complet du run
- Métriques clés pour $DESIGN :
  - `#OBJV` — valeur de l'objectif
  - `RELATIVEINF(%)` — information relative par paramètre (ETA shrinkage type 11)
  - `DESIGN TYPE: D-OPTIMALITY, -LOG(DET(FIM))`
  - `Elapsed opt. design time in seconds`
  - `EBVSHRINKSD(%)`, `EBVSHRINKVR(%)`

### Fichier `.tab`
- Tableau utilisateur défini via `$TABLE`
- Après optimisation : contient les temps/doses optimisés (TIME, TSTRAT, IPRED...)

### Fichier `.shk`
- Shrinkage en format colonnes
- Type 11 = `RELATIVEINF(%)`

### Fichier `.bfm`
- Uniquement avec `OFVTYPE=8` (Bayes FIM)
- Progression des variances conditionnelles individuelles pendant l'optimisation

### Fichier `.vpd` / `.vpt`
- Variance-covariance des paramètres individuels et population

---

## Options $DESIGN — référence complète

### Critère d'optimalité (`OFVTYPE=`)
| Valeur | Critère | Notes |
|--------|---------|-------|
| 0/1/3/4/5 | D-optimality : `-log(det(FIM))` | Défaut |
| 2 | A-optimality : `-1/tr(FIM⁻¹)` | |
| 6 | DS-optimality : `-log(det(FIM)) + log(det(FIM_unint))` | Utiliser `UNINT` |
| 7 | R-optimality (RSE) : `-1/tr(√(FIM⁻¹)/θ)` | |
| 8 | Bayesian FIM individuel : `-log(det(FIM_bayes))` | Fichier `.bfm` |
| 9/10 | A/R avec filtre UNINT | |

### Type de FIM (`FIMTYPE=` ou `FIMDIAG=`)
| Valeur | Description | Équivalent |
|--------|-------------|----------|
| 0 | FIM complète, diff. finies pour tout | POPED fim.calc.type=0 |
| 1 | Bloc-diagonal, analytique si MU-ref | POPED fim.calc.type=1 / PFIM diagonal |
| 2 | Bloc, sans hypothèse indépendance C(θ) | POPED fim.calc.type=2 |
| 3 | Complète + analytique pour Σ×Σ | POPED fim.calc.type=3 |

`FIMTYPE=1` + `VARCROSS=1` = POPED `fim.calc.type=4` = PFIM diagonal

### Algorithmes d'optimisation
| Keyword | Méthode |
|---------|----------|
| `NELDER` | Nelder-Mead (risque minima locaux) |
| `RS` | Random Search |
| `STGR` | Gradient stochastique |
| `FEDOROV` | Exchange algorithm (temps discrets) |
| `DISCRETE` | Optimise nombre + positions (via NELDER) |
| `DISCRETE_RS` / `DISCRETE_SG` | Idem via RS/STGR |

### Autres options importantes
- `GROUPSIZE=N` — nombre de sujets (multiplie la FIM)
- `APPROX=FO/FOI/FOCE/FOCEI/LAPLACE` — approximation NLME (FO par défaut)
- `DESEL=TIME/AMT` + `DESELSTRAT/DESELMIN/DESELMAX` — éléments à optimiser
- `STRAT/STRATF` — optimise la proportion de sujets par groupe
- `NMIN/NMAX` — bornes sur le nombre de points (DISCRETE uniquement)
- `VARCROSS=0/1` — modèle erreur résiduelle (1 = style PFIM)
- `EOPTD=1` — FIM espérée sur l'incertitude paramétrique (avec `$PRIOR`)
- `NOHABORT` — ne pas avorter sur problème Hessien
- `POSTHOC` — calcul post-hoc des ETAs
- `SIGL=10/12` — chiffres significatifs
- `CLOCKSEED=1` — seed horloger (réplications)
- `MAXEVAL=0` — évaluation; `MAXEVAL>0` — optimisation
- `MODE=0/2` — pour APPROX=FOCEI (MODE=1 déconseillé)
- `GRD=TS(n)` dans `$EST` — THETA sigma-like (résiduelle avec FIMDIAG=1)

### Design séquentiel : `$RCOVI`
```
$RCOVI FILE=previous_run.coi
$DESIGN GROUPSIZE=50 FIMTYPE=1 MAXEVAL=9999 ...
```
- Lit la FIM d'un run précédent et l'**additionne** à la FIM courante
- Principe : FIM_totale = FIM_étude_A + FIM_étude_B (observations indépendantes)
- Cas d'usage : Phase I→II, design adaptatif, info historique, multi-centres
- `$RCOV FILE=run.cov` — même chose depuis le fichier `.cov` (variance-covariance)

---

## Exemples de référence (Bauer 2021)

| Exemple | Dossier | Ce qu'il illustre |
|---------|---------|-------------------|
| 1 | `docs/bauer2021_examples/example1/warfarin.*` | Évaluation simple, FIM bloc-diag |
| 2 | `docs/bauer2021_examples/example2/warfarin2.*` | Optimisation temps, NELDER |
| 3 | `docs/bauer2021_examples/example3/priortrue.*` | Robust design via $SIM TRUE=PRIOR |
| 4 | `docs/bauer2021_examples/example4/warfarin_pkpd_*` | PK-PD multi-réponses |
| 5 | `docs/bauer2021_examples/example5/optdesign2.*` | DS-optimality, UNINT |
| 6 | `docs/bauer2021_examples/example6/tmdd2.*` | TMDD, STRAT/STRATF, ODE |
| 7 | `docs/bauer2021_examples/example7/tmdd2b.*` | Bayes FIM, optimisation dose+temps |
