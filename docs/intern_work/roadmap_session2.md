# Bilan & Roadmap — Après session 1-2
*Rédigé le 2026-03-08*

---

## Ce qui a été accompli

### Compréhension de $DESIGN
- Lecture complète du manuel NONMEM (sections I.34–I.73)
- Lecture du papier Bauer 2021 + suppléments mathématiques
- Étude des 7 exemples complets (warfarin, TMDD, Bayes FIM, designs multi-groupes)
- Maîtrise des options clés : FIMTYPE, OFVTYPE, DESEL, STRAT, VARCROSS, APPROX, EOPTD
- Compréhension des fichiers de sortie : .ext, .coi, .clt, .shk, .bfm, .vpd

### Outils développés
- `scripts/parse_design_outputs.R` : parsers complets (read_ext, read_shk, get_rse, get_relativeinf, get_ofv, get_se, get_final_params, summary_design)
- `scripts/report_design.R` : visualisations (plot_relativeinf, plot_rse, plot_convergence)

### Document de thèse
- Ébauche structurée : intro, théorie, outils, question de recherche, méthodologie 3 phases
- Question de recherche claire et opérationnelle
- Lacunes identifiées : section métriques vide, planning absent

---

## Évaluation honnête du niveau actuel

### Points solides
- Vue d'ensemble de $DESIGN très complète (plus que la plupart des utilisateurs)
- Compréhension des fichiers de sortie et de leur structure
- Sens pratique : retour d'expérience déjà intégré dans la thèse
- Scripts R fonctionnels et bien structurés

### Points à approfondir
1. **Mathématiques de la FIM** — la structure en blocs (A, B, C) et les conditions de validité de l'approximation bloc-diagonale méritent d'être vraiment maîtrisées, pas juste connues
2. **Méthodes d'estimation NLME** — FOCE vs FOCEI, EM methods : connaissances théoriques à consolider
3. **Comparaison $DESIGN vs PFIM/PopED** — pas encore faite concrètement
4. **SSE** — mentionnées dans la thèse mais pas encore réalisées
5. **Métriques de design** — section vide dans la thèse, concept encore flou

---

## Roadmap suggérée

### Prochainement (mars)
1. **Quiz session 2** : identifier précisément les lacunes (`quiz_session2.md`)
2. **Lire le cours NLME** (`courses/nlme.pdf`) pour consolider les bases théoriques
3. **Remplir la section métriques** dans la thèse (RSE seuils, D-efficiency, relative information)
4. **Évaluation Phase 1** : appliquer $DESIGN au modèle frexalimab réel (évaluation du design existant)

### Avril
5. **SSE sur 2-3 scénarios** pour valider les prédictions FIM
6. **Comparaison PFIM ou PopED** sur un scénario simple warfarin pour cross-valider
7. **Optimisation Phase 2** : D-optimal design pour frexalimab avec contraintes cliniques

### Mai–Juin
8. **Design robuste** : analyse de sensibilité aux paramètres prior
9. **Exploration PUMAS** (secondary endpoint)
10. **Rédaction méthodologie** : workflow formalisé + métriques opérationnelles

### Fil conducteur
- Scripts R : affiner au fur et à mesure des besoins (Shiny ?)
- Thèse : rédiger une section dès que quelque chose est consolidé (ne pas tout laisser pour la fin)

---

## Cours disponibles à exploiter

| Fichier | Contenu attendu | Priorité |
|---------|----------------|---------|
| `courses/nlme.pdf` | Modèles NLME — fondations théoriques | 🔴 Haute |
| *(à venir)* | Optimal design ? | À définir |

---

## Questions ouvertes à résoudre

1. **Disponibilité des données frexalimab** : quand peut-on commencer Phase 1 ? (dépend de l'accès)
2. **Resources de calcul SSE** : combien de CPUs disponibles pour 200 réplicats NONMEM ?
3. **Scope PUMAS** : exploration légère ou évaluation formelle ?
4. **Section métriques** : quels critères retenir ? (RSE < 30% ? D-efficiency > 80% ? Relative information ?)
5. **Elisa** : discussion sur l'analyse de sensibilité (mentionnée dans la thèse comme "à voir avec Elisa")
