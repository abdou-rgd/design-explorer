# Quiz de connaissances — Session 2
## NLME / Optimal Design / FIM / NONMEM $DESIGN

> **Protocole** : réponds sans notes. Indique ton niveau de confiance : ✓ sûr / ~ incertain / ✗ je ne sais pas. On corrige ensemble et on identifie les lacunes.
>
> 🟢 = directement dans le cours `nlme.pdf` (Leroux, janv. 2026)
> 🟡 = cours + tes lectures Bauer/manuel NONMEM
> 🔴 = au-delà du cours, issu de ta pratique du stage

---

## Bloc A — Modèles NLME : fondations 🟢

**A1.** Écris le modèle NLME général pour un individu *i* avec *K* réponses. Définis chaque terme ($Y_i$, $f_k$, $\theta_i$, $\varepsilon_i$, $\Sigma_i$) et la structure de $\Sigma_i$ pour un modèle multi-réponses.

**A2.** Donne les trois modèles d'erreur résiduelle vus dans le cours (homoscédastique, hétéroscédastique, combiné). Pour chacun, écris $\text{Var}(y_{ik})$ et donne un contexte où il est approprié.

**A3.** Écris le modèle IIV log-normal ($\theta_i = \mu \cdot e^{\eta_i}$). Pourquoi préfère-t-on ce modèle pour CL et V ? Cite deux autres distributions possibles pour des paramètres avec contraintes différentes.

**A4.** Qu'est-ce que le "biais" et l'"imprécision" dans l'estimation des paramètres ? Comment les visualise-t-on ? Quel est l'objectif de SE en PopPK (seuil du cours) ?

**A5.** Définis le design élémentaire $\xi_i$ et le design de population $\Xi$. Comment passe-t-on d'un design théorique à un design pratique avec Q bras (arms) ? Donne la formule pour $n_q$.

---

## Bloc B — Matrice d'Information de Fisher : théorie 🟢

**B1.** Donne la définition formelle de la FIM pour un design de population à Q bras. Comment la FIM totale se décompose-t-elle en contributions individuelles ?

**B2.** Énonce la borne de Cramér-Rao. Qu'implique-t-elle pour le calcul des SE prédictes ? Quelle approximation fait-on en "planification locale" ?

**B3.** La FIM a une structure diagonale par blocs **A** et **B**. Que contient chaque bloc ? Écris les expressions de $A_{lm}$ et $B_{lm}$ du cours.

**B4.** Quelle est la conséquence de $\text{SE} \propto 1/\sqrt{N}$ pour la stratégie de design ? Qu'est-il souvent plus efficace de faire plutôt que d'augmenter $N$ ?

**B5.** Pourquoi la FIM peut-elle être singulière ? Qu'est-ce que cela signifie en pratique pour l'estimation ?

**B6.** *(Dérivation)* Le cours montre que $\text{Var}(Y_i) \approx J_{\theta F} \cdot J_{\phi h^{-1}} \cdot \Omega \cdot (J_{\theta F} \cdot J_{\phi h^{-1}})^\top + \Sigma_i$. Explique le rôle de chacun des deux termes. Que représente physiquement chaque Jacobienne ?

---

## Bloc C — Linéarisation et approximation 🟢🟡

**C1.** Pourquoi faut-il linéariser le modèle NLME pour calculer la FIM ? Autour de quel point fait-on la linéarisation au premier ordre (FO) ?

**C2.** *(Hors cours, pratique NONMEM)* Quelle est la différence entre FO, FOCE et FOCEI ? Dans quel contexte FOCEI est-il nécessaire ? Quel est l'impact du choix de l'approximation sur les résultats de $DESIGN ?

---

## Bloc D — Critères d'optimalité et algorithmes 🟢

**D1.** Définis le critère D-optimal : donne la formule de $\Phi_D(\Xi)$ telle qu'elle apparaît dans le cours. Quelle quantité géométrique minimise-t-il ?

**D2.** Comment compare-t-on deux designs $\Xi_A$ et $\Xi_B$ avec le critère D ? Si le ratio des critères vaut 2, qu'est-ce que cela signifie concrètement ?

**D3.** Quelle est la différence entre un design "exact" et un design "statistique" ? Entre un espace de recherche "discret" et "continu" ?

**D4.** Cite les 4 algorithmes d'optimisation du cours. Lequel est global (échappe aux maxima locaux) ? Lequel est adapté à l'allocation discrète ? Lequel est implémenté dans NONMEM $DESIGN sous le nom `NELDER` ?

**D5.** Quelle est la limitation commune aux algorithmes Multiplicatif et Fedorov-Wynn ? Et à Nelder-Mead ? Comment la dépasse-t-on en pratique ?

---

## Bloc E — $DESIGN NONMEM en pratique 🟡🔴

**E1.** Quelles sont les deux grandes utilisations de `$DESIGN` ? Comment les distingue-t-on dans le control stream (option clé) ?

**E2.** Explique `FIMTYPE=` (0 vs 1). Quelle condition dans le modèle `$PK` rend `FIMTYPE=1` valide et rapide ? Comment cela se traduit-il en NONMEM ?

**E3.** Dans un fichier `.ext` de run `$DESIGN`, que contient la ligne d'itération `-1000000000` ? Et `-1000000001` ?

**E4.** Qu'est-ce que la `RELATIVEINF(%)` dans les sorties NONMEM (fichier `.shk` type 11) ? Comment l'interpréter pour un paramètre donné ?

**E5.** Tu veux optimiser les temps de prélèvements entre 0 et 24h. Écris les 4 options `$DESIGN` clés à spécifier (`DESEL`, `DESELSTRAT`, `DESELMIN`, `DESELMAX`).

**E6.** Qu'est-ce que le design séquentiel avec `$RCOVI` ? Quelle propriété mathématique de la FIM le rend possible ?

---

## Bloc F — Validation et méthodes alternatives 🟡

**F1.** Décris le principe des SSE (Stochastic Simulation & Estimation) en 4 étapes. Pourquoi sont-elles le "gold standard" malgré leur coût ?

**F2.** Dans quelles situations la NCA est-elle insuffisante pour évaluer un design ? La FIM apporte-t-elle une réponse dans ces cas — et à quelle condition ?

**F3.** Cite deux différences majeures entre $DESIGN NONMEM et PFIM/PopED pour réaliser la même tâche.

---

## Bloc G — Questions ouvertes 🔴

**G1.** Un pharmacométricien demande : "Pourquoi ne pas simplement prendre beaucoup de prélèvements ?" Réponds en 3-4 phrases en mentionnant les contraintes éthiques, logistiques, et le rôle de la FIM.

**G2.** Cite 2 limitations majeures de `$DESIGN` NONMEM que tu as identifiées pendant ton stage. Comment les documenterais-tu dans ta méthodologie ?

**G3.** *(Réflexion)* Ton modèle frexalimab a des ω² élevés (forte IIV). Comment cela affecte-t-il la FIM et les RSE prédits ? Que devrait-on faire dans le design pour y remédier ?

---

## Grille d'auto-évaluation

| Bloc | Thème | Nb questions | Score | Points faibles |
|------|-------|-------------|-------|----------------|
| A | NLME fondations 🟢 | 5 | /5 | |
| B | FIM théorie 🟢 | 6 | /6 | |
| C | Linéarisation 🟢🟡 | 2 | /2 | |
| D | Critères & algo 🟢 | 5 | /5 | |
| E | $DESIGN pratique 🟡🔴 | 6 | /6 | |
| F | Validation 🟡 | 3 | /3 | |
| G | Ouvertes 🔴 | 3 | /3 | |
| **Total** | | **30** | **/30** | |

---

*Calibré sur le cours `docs/courses/nlme.pdf` (Romain Leroux, 17 janvier 2026, 38 pages) — mis à jour le 2026-03-08.*
