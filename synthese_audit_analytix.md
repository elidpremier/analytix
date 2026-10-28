# Audit du package `analytix` 0.6.0.9000 — rapport de synthèse

**Cible** : `elidpremier/analytix`, branche `main`, commit `35b69f42cf309d7ca2c29aa57940ca6c3ec76fc7`
**Version auditée** : 0.6.0.9000 · **Date de l'audit** : 23 octobre 2026
**Environnement** : R 4.5.0, Debian 13, locale `C.utf8`, `analytix` installé depuis les sources
**Périmètre** : 73 fonctions canoniques, 54 alias dépréciés, 127 exports (`NAMESPACE`)
**Mode** : lecture seule — aucun fichier du dépôt n'a été modifié

---

## 1. Résumé exécutif

L'audit a exécuté **937 cas de test** (dont 413 cas de balayage systématique des arguments) et
rédigé **23 constats** documentés, chacun avec un exemple reproductible et un test de non-régression.

| Verdict | Nombre | Part |
|---|---:|---:|
| PASS | **794** | 84,7 % |
| FAIL | **101** | 10,8 % |
| SKIP (non exerçable automatiquement) | **42** | 4,5 % |

**Cinq constats bloquants (S0)** doivent être corrigés avant toute diffusion :

1. **F-01** — `analytix_sample_data()` renvoie une colonne `age` **constante** (95 ans pour toutes
   les observations) : tout le jeu de démonstration du package est statistiquement vide.
2. **F-22** — `prep_recode_odk()` **fabrique des réponses** : `NA` → « Non », valeur hors vocabulaire
   → « Oui », sans avertissement (2 valeurs fabriquées sur 7 dans le cas de test).
3. **F-09** — `export_word(env = TRUE)` exporte **tout `globalenv()`**, y compris des données
   nominatives sans rapport avec l'analyse.
4. **F-10** — `.http_post()` (repli sans `curl`) construit une ligne de commande shell avec les
   en-têtes **non échappés** : une apostrophe dans une valeur de clé exécute une commande arbitraire
   (**démontré**, fichier témoin créé).
5. **F-11** — `.Renviron` est écrit en **0644** (clés lisibles par tout compte) et `readRenviron()`
   importe dans la session des variables étrangères au package.

**Principaux constats majeurs (S1)** : `plot_km()` ne fonctionne **jamais** (F-05) ; `tbl_logistic()`
exponentie un modèle de Poisson et publie un RR sous l'étiquette « OR » (F-03) ; `tbl_bivariate_or()`
discrétise silencieusement les expositions continues et publie des OR de séparation parfaite à
180 chiffres (F-04) ; `stat_sens_spec()` affiche « sensibilité 0,0 % » quand le dénominateur est nul
(F-06) ; `clean_numeric()` détruit des valeurs sans compte rendu (F-07) ; `plot_pie()` échoue au-delà
de 7 modalités (F-08) ; la p-value de Fisher de `tbl_cross_unique()` change à chaque appel (F-02).

**Verdict global** : l'API est riche et la couverture documentaire bonne (127 exports, 54 alias
documentés), mais **la couche statistique n'est pas fiable en l'état** : plusieurs fonctions
produisent des nombres faux, non reproductibles ou fabriqués, sans le moindre signal. La priorité
n'est pas cosmétique : elle porte sur l'exactitude (F-01, F-02, F-03, F-04, F-06, F-22) et sur la
sécurité de la chaîne « clé API / export » (F-09, F-10, F-11).

---

## 2. Périmètre, méthode et traçabilité

| Work package | Contenu | Artefact |
|---|---|---|
| WP0 | Environnement, installation propre, `R CMD check` sans tests | `00_env/sessionInfo.txt`, `logs/check_noTests.log` |
| WP1 | Matrice d'audit `(fonction, argument)` | `audit_matrix.csv` (510 lignes : 456 canoniques + 54 alias) |
| WP2 | Jeux de test indépendants (aucun jeu du package en entrée des oracles) | `fixtures/` + `MANIFEST.csv` |
| WP3 | Oracles indépendants (`stats`, `pROC`, `mice`, `binom`, `survdiff`…) | colonne `oracle` de `results.csv` |
| WP4 | Cas par famille (préparation, univarié, bivarié, graphiques, export, IA/sécurité, alias, transversal) | `cases/10…80*.R` |
| WP5 | Balayage systématique de tous les couples de la matrice | `cases/90_couverture.R` (455 cas) |
| WP6 | Sécurité, confidentialité, effets de bord, cohérence inter-fonctions | `cases/60_ia_securite.R`, `cases/80_transversal.R`, `R/05_doc_checks.R` |
| WP7 | Validation du harnais lui-même par injection de mutations | `R/06_mutation.R`, `R/07_mutation_detect.R` |

**Reproductibilité complète** :
```bash
cd /home/user/audit
LANG=C.utf8 Rscript R/99_run_all.R            # 937 cas -> results/results.csv
LANG=C.utf8 Rscript R/05_doc_checks.R         # -> results/doc_checks.rds
LANG=C.utf8 Rscript R/06_mutation.R           # -> work/mutant
mkdir -p work/mutlib && LANG=C.utf8 LC_ALL=C.utf8 R CMD INSTALL -l work/mutlib work/mutant
LANG=C.utf8 R_LIBS=/home/user/audit/work/mutlib Rscript R/07_mutation_detect.R
LANG=C.utf8 Rscript -e 'testthat::test_dir("tests_nonregression")'
```

---

## 3. Couverture

### 3.1 Matrice d'audit

| Indicateur | Valeur |
|---|---|
| Couples `(fonction, argument)` canoniques exercés | **456 / 456 (100 %)** |
| Fonctions canoniques exercées | **73 / 73 (100 %)** |
| Alias dépréciés exercés (avertissement + équivalence) | **54 / 54 (100 %)** |
| Cas exécutés au total | **937** |

L'interface Shiny, qui ne rend pas la main, a été exercée dans un processus fils puis sondée par HTTP
(`cases/85_gui.R`) : `run_gui()` et son alias `run_analytix_ui()` répondent tous deux correctement.

### 3.2 Résultats par famille

| Famille | Portée | PASS | FAIL | SKIP |
|---|---|---:|---:|---:|
| PREP — préparation des données | import, nettoyage, recodage, imputation, outliers | 57 | **17** | 0 |
| UNI — analyse univariée | `desc_*` (9 fonctions) | 58 | **13** | 0 |
| BIV — analyse bivariée | OR, logistique, khi²/Fisher, ANOVA, corrélation, ROC, KM | 54 | **25** | 0 |
| COU — balayage systématique | 455 couples de la matrice | 413 | 0 | 42 |
| EXP — export et rapports | Word, thèmes, `report_*` | 31 | **15** | 0 |
| PLT — graphiques | `plot_*` (11 fonctions) | 26 | **9** | 0 |
| SEC — sécurité / IA / H1–H12 | injections, clés, graine, alias, dates | 25 | **17** | 0 |
| TRA — transversal | cohérence, invariants, pureté, format | 22 | **2** | 0 |
| ALI — alias | 54 alias × 2 vérifications | 105 | **3** | 0 |
| GUI — interface Shiny | démarrage, port, alias | 3 | 0 | 0 |
| **Total** | | **794** | **101** | **42** |

Les 42 SKIP sont analysés en §7 ; 21 d'entre eux sont couverts par un cas dédié d'une autre famille
(par exemple `output=`/`env=` couverts par EXP et SEC), 21 restent hors balayage automatique.

### 3.3 Validation du harnais (WP7)

Cinq mutations ont été injectées dans une copie du paquet, puis le paquet mutant a été installé et
soumis aux sondes de détection :

| Mutation | Nature | Détectée |
|---|---|---|
| M1 | pourcentage multiplié par 100 deux fois (`.fmt_decimal`) | oui |
| M2 | marges ligne/colonne inversées (`prop.table`) | oui |
| M3 | effectif de cas incrémenté (`calc_prevalence`) | oui |
| M4 | inversion de l'événement (`bivariate_or_table`) | oui |
| M5 | variance au lieu de l'écart-type (`descr_numeric`) | oui |

**5 / 5 détectées sur le mutant, 0 / 5 signalées sur le paquet sain** (`results/mutation_detection.csv`) :
le harnais ne produit pas de faux positifs et détecte les défauts numériques silencieux.

### 3.4 Jeu de non-régression livré

`tests_nonregression/` : **11 fichiers, 43 tests, 62 assertions, 0 échec, 1 saut assumé**
(application Shiny non simulable sans casser l'installation). Chaque test correspond à un constat ou
à une fonction qui n'était couverte par aucun test du paquet ; ceux qui décrivent un défaut sont
commentés « défaut F-xx » et devront être inversés après correctif.

Une analyse statique des tests livrés avec le paquet montre que **29 des 73 fonctions canoniques ne
sont citées dans aucun test** ; la suite proposée en couvre **27** (les 2 restantes — `run_gui`,
`run_analytix_ui` — sont couvertes par un test de démarrage HTTP réel).

---

## 4. Constats par gravité

### 4.1 Bloquants (S0) — 5

| Id | Fonction / argument | Défaut | Preuve |
|---|---|---|---|
| **F-01** | `analytix_sample_data(n)` | colonne `age` constante (95 ans) | `length(unique(age)) == 1` |
| **F-09** | `export_word(env = TRUE)` | exporte tout `globalenv()` | données nominatives exportées |
| **F-10** | `.http_post(headers)` | injection de commande shell | fichier témoin créé |
| **F-11** | `setup_api_keys(path)` | `.Renviron` en 0644 + import implicite de variables | `mode == 420` |
| **F-22** | `prep_recode_odk(x)` | `NA` → « Non », hors vocabulaire → « Oui » | 2 valeurs fabriquées / 7 |

### 4.2 Majeurs (S1) — 8

| Id | Fonction / argument | Défaut |
|---|---|---|
| F-02 | `tbl_cross_unique(test)` | p-value de Fisher simulée sans graine : change à chaque appel (0,020 → 0,024) |
| F-03 | `tbl_logistic(model)` | accepte tout `glm` (Poisson, quasi) et publie un RR comme « OR » ; perd une covariable sans intercept |
| F-04 | `tbl_bivariate_or(exposures, outcome_positive_val, conf_level)` | discrétise les expositions continues ; publie des OR de séparation à 180 chiffres ; ne valide ni la valeur d'issue ni `conf_level` |
| F-05 | `plot_km()`, `km_plot()` | échec systématique : `object 'surv_obj' not found` (pile d'appel : `surv_pvalue → get_all_vars`) |
| F-06 | `stat_sens_spec(actual, predicted)` | `NA` supprimés sans mention ; sensibilité 0,0 % sur dénominateur nul |
| F-07 | `clean_numeric()`, `clean_text()`, `clean_names()` | destruction silencieuse de valeurs (formats FR, espaces fines) |
| F-08 | `plot_pie(x)` | échec au `build` au-delà de 7 modalités (palette de 7 couleurs codée en dur) |
| F-23 | `llm_summarize_table(n_sentences)` | argument sans effet (prompt identique pour 1 et 8 phrases) |

### 4.3 Modérés (S2) — 8

| Id | Fonction / argument | Défaut |
|---|---|---|
| F-12 | `prep_impute_mice(seed)`, `analytix_sample_data(seed)` | `.Random.seed` non restaurée |
| F-13 | `prep_outliers`, `prep_import`, `desc_*` | erreurs internes non traduites sur entrées invalides |
| F-14 | `tbl_cross_unique(include_na)` | `subscript out of bounds` ; `NA` non comptés sans l'option |
| F-15 | `tbl_correlation(cols)` | colonnes non numériques écartées sans avertissement |
| F-16 | `tbl_roc`, `theme_analytique` | structure de retour hétérogène (pas de `$data`) ; `theme_analytique()` accepte un `data.frame` |
| F-17 | 54 alias | double avertissement (`format_flextable`), signature `(...)`, un avertissement par appel |
| F-20 | `analytix_sample_data(n)` | prévalence de décès 86,5 %, issue non libellée, `n = 0` en erreur interne |
| F-21 | `tbl_km(time_var, group_var)` | structures de sortie différentes avec/sans groupe ; temps négatifs acceptés ; quartiles faux si non atteints |

### 4.4 Mineurs (S3) — 2

| Id | Objet | Défaut |
|---|---|---|
| F-18 | README, DESCRIPTION, `API_COMPATIBILITY.md` | `colorspace` cité mais non déclaré, `bslib`/`DT`/`shinyjs`/`bsicons` déclarés mais non utilisés, `descr_categorial` encore dans le README |
| F-19 | libellés, format, dates | « Mediane »/« Ecart_type » sans accents, décimales non formatées dans `format()`, dates en anglais |

### 4.5 Constats par famille

| Famille | Constats | S0 | S1 | S2 | S3 |
|---|---|---:|---:|---:|---:|
| PREP — préparation | F-07, F-12, F-13, F-22 | 1 | 1 | 2 | 0 |
| UNI — univarié | F-06, F-12, F-13, F-19, F-21 | 0 | 1 | 3 | 1 |
| BIV — bivarié | F-02, F-03, F-04, F-06, F-14, F-15, F-16, F-21 | 0 | 4 | 4 | 0 |
| PLT — graphiques | F-05, F-08, F-16 | 0 | 2 | 1 | 0 |
| EXP — export / rapports | F-09, F-19 | 1 | 0 | 0 | 1 |
| SEC — sécurité / IA | F-10, F-11, F-12, F-17, F-23 | 2 | 1 | 2 | 0 |
| Données d'exemple | F-01, F-20 | 1 | 0 | 1 | 0 |
| Documentation | F-18 | 0 | 0 | 0 | 1 |

*(un constat peut concerner plusieurs familles ; la colonne « Constats » liste tous les constats
ouverts par la famille, les colonnes S0–S3 comptent les constats **dont cette famille est le siège
principal**.)*

---

## 5. Hypothèses H1 à H12 — verdicts

| # | Hypothèse | Verdict | Preuve |
|---|---|---|---|
| H1 | `.http_post()` permet une injection shell | **Confirmée** (voie de repli sans `curl`) | `SEC-H1-01` ; commande tracée ; `touch /tmp/PWNED_INJ` exécuté. URL et corps correctement protégés (H1-02, H1-03) |
| H2 | `report_express(use_ai = TRUE)` par défaut est risqué | **Partielle** | Sans clé, aucun appel réseau : repli local annoncé (`SEC-H2-04`, `H2-05`) ; prompt = agrégats uniquement (`H2-02`) ; en revanche `n_sentences` sans effet (`H2-03` → F-23) |
| H3 | `setup_api_keys()` réécrit `.Renviron` sans précaution | **Confirmée** | Droits 0644, apostrophe cassant le fichier, `readRenviron()` important des variables tierces |
| H4 | `.get_api_key()` lit le `.Renviron` du répertoire courant | **Confirmée** | `PATH_AUDIT` passe de `""` à `"42"` dans la session |
| H5 | `report_magic(open_doc = TRUE)` lance un processus externe | **Confirmée** | Sous `Rscript` : `xdg-open: file … does not exist` (processus lancé, échec silencieux) |
| H6 | `export_word(env = TRUE)` parcourt `globalenv()` | **Confirmée** | Objet nominatif exporté → F-09 |
| H7 | `clean_names()` et `%>%` exportés créent un risque de masquage | **Confirmée** | Conflit avec `janitor::clean_names`, masquage de magrittr/dplyr (`SEC-H7-01…03` PASS, défaut = risque documenté) |
| H8 | La graine de l'utilisateur n'est pas restaurée | **Confirmée** | `identical(avant, .Random.seed)` → `FALSE` (2 fonctions) |
| H9 | Les dates dépendent de la locale | **Confirmée** | « 23 October 2026 » en locale `C`, dans `export_tables()` et `report_generate()` |
| H10 | `plot_km()` dépend de `survminer` sans garde | **Confirmée** | Message d'absence absent (`SEC-H10-02` FAIL) ; avec `survminer` installé, échec sur `surv_obj` → F-05 |
| H11 | Les alias avertissent à chaque appel et masquent leur signature | **Confirmée** | 3 appels → 3 avertissements ; `args()` renvoie `...` ; `format_flextable` en émet 2 |
| H12 | `analytix_sample_data()` a une colonne `age` constante | **Confirmée** | `length(unique(age)) == 1` ; `n = 0` en erreur interne |

---

## 6. Contrôles documentaires (WP6)

Source : `results/doc_checks.rds` (script `R/05_doc_checks.R`).

| Contrôle | Résultat |
|---|---|
| Paquets cités dans le README | `colorspace` cité **1 fois**, absent de `DESCRIPTION` et de tout `pkg::` |
| Dépendances déclarées jamais utilisées | `DT`, `shinyjs`, `bsicons` (aucun `pkg::` dans `R/`) ; `bslib` cité dans le README mais jamais appelé |
| Paquet utilisé mais non déclaré | aucun `scales` (alors qu'il serait nécessaire pour corriger F-08) |
| Anciens noms dans le README | `descr_categorial(` : 1 occurrence (les 53 autres alias : 0) |
| Promesse « *n* fonctions » dans le README | **absente** — soupçon infirmé |
| Cohérence `NAMESPACE` | 127 exports = 73 canoniques + 54 alias |
| `NEWS.md` | 4 / 4 entrées correspondent à des fonctions existantes |
| `API_COMPATIBILITY.md` | 54 / 54 alias documentés |
| Exports Word relus | `officer::docx_summary()` : nombre de tableaux = `length(tables)`, XML bien formé, contenu des cellules = `$data` (familles EXP et ALI) |

---

## 7. Voies d'ajustement — six axes, classés par rapport impact / effort

### Axe 1 — Exactitude statistique *(priorité absolue)*

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 1.1 | Corriger `pmin()` dans `analytix_sample_data()` et régénérer le jeu d'exemple (prévalence de décès réaliste, issue en facteur libellé) | F-01, F-20 | **très élevé** | **faible** (2 lignes) |
| 1.2 | Recodage strict : `NA` → `NA`, hors vocabulaire → `NA` + avertissement (aligner `prep_recode_odk` sur `clean_binary`) | F-22 | **très élevé** | faible |
| 1.3 | Valider `outcome_positive_val`, `conf_level`, `family(model)`, `n`, `time ≥ 0` en tête de fonction | F-03, F-04, F-20, F-21 | très élevé | faible |
| 1.4 | Expositions continues : OR par unité (ou refus explicite) ; détecter la séparation parfaite et publier `NA` + avertissement | F-04 | élevé | moyen |
| 1.5 | Dénominateurs nuls → `NA` (et non 0) dans `stat_sens_spec` ; signaler les observations exclues | F-06 | élevé | faible |
| 1.6 | Fisher r×c exact par défaut ; si simulation, graine locale et `B` exposé | F-02 | élevé | faible |
| 1.7 | Restaurer `.Random.seed` (`withr::with_seed` ou `on.exit`) dans toute fonction qui tire au sort | F-12 | moyen | faible |

### Axe 2 — Robustesse des arguments

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 2.1 | Validateur commun `.check_var(data, var, arg)` produisant des messages en français nommant la colonne attendue et les colonnes disponibles | F-13 | élevé | moyen |
| 2.2 | `clean_numeric()` : reconnaître l'espace insécable (U+00A0), l'espace fine (U+202F), le `%` ; avertir du nombre de valeurs converties en `NA` | F-07 | élevé | moyen |
| 2.3 | `tbl_cross_unique(include_na = TRUE)` : construire les niveaux avec `addNA()` ; sans l'option, afficher le nombre d'exclusions | F-14 | moyen | faible |
| 2.4 | `tbl_correlation()` : avertir des colonnes écartées, refuser une sélection vide | F-15 | moyen | faible |
| 2.5 | `analytix_sample_data(n)` : refuser `n < 2` et `n` non entier par un message explicite | F-20 | moyen | faible |

### Axe 3 — Cohérence de l'API (nu/chaîne, alias, signatures)

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 3.1 | Contrat de retour unique : `new_analytix_table(data, flextable, meta)` utilisé par tous les `tbl_*` (`tbl_roc` inclus) | F-16 | élevé | moyen |
| 3.2 | Accepter partout les noms de colonnes sous forme de chaîne **et** de symbole nu (NSE), documenté | F-13, F-16 | moyen | moyen |
| 3.3 | Réduire la dépréciation en une étape : `format_flextable()` → `theme_analytique()` directement, un avertissement par session | F-17 | moyen | faible |
| 3.4 | Signatures explicites (ou `...` documenté) pour les 54 alias | F-17 | moyen | moyen |
| 3.5 | `plot_km()` : construire la formule dans un `data.frame` local pour que `survminer` retrouve ses variables | F-05 | élevé | faible |

### Axe 4 — Sorties et rapports

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 4.1 | Palette dynamique (`hcl.colors`) et vérification `ggplot_build()` interne avant retour | F-08 | élevé | faible |
| 4.2 | `tbl_km` : structure de sortie identique avec/sans `group_var`, quartiles `NA` si non atteints, refus des temps négatifs | F-21 | moyen | faible |
| 4.3 | Dates en français indépendamment de la locale (mois codés en dur) | F-19 | moyen | faible |
| 4.4 | Libellés accentués partagés (`LIBELLES_STATS`), `.fmt_decimal()` appliqué dans `format()` | F-19 | moyen | faible |
| 4.5 | Relire chaque `.docx` produit (`docx_summary`) dans les tests du paquet | — | moyen | faible |

### Axe 5 — Sécurité et confidentialité

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 5.1 | Supprimer le repli `system()` de `.http_post()` : `curl` en dépendance dure ou `download.file(headers = …)` | F-10 | **très élevé** | faible |
| 5.2 | `.Renviron` en 0600, écriture atomique, valeurs échappées ; ne plus importer de variables tierces | F-11 | **très élevé** | faible |
| 5.3 | `export_word(env = TRUE)` limité aux objets `analytix_table`, avec liste annoncée ; `env` déprécié | F-09 | **très élevé** | faible |
| 5.4 | `report_magic(open_doc = FALSE)` par défaut, ouverture conditionnée à `interactive()` | H5 | moyen | faible |
| 5.5 | Test de non-régression anti-`system(` et test d'échappement des en-têtes | F-10 | élevé | faible |

### Axe 6 — Documentation et maintenabilité

| # | Proposition | Constats | Impact | Effort |
|---|---|---|---|---|
| 6.1 | README : remplacer `descr_categorial` par `desc_categorical`, retirer `colorspace`, aligner les dépendances déclarées | F-18 | moyen | faible |
| 6.2 | Ajouter les tests manquants : le paquet en compte 29 fonctions non citées ; intégrer `tests_nonregression/` | couverture | élevé | moyen |
| 6.3 | Documenter explicitement la méthode d'IC (Wald / Wilson / profil) dans chaque en-tête de tableau | F-04, F-06 | moyen | faible |
| 6.4 | Intégrer les 5 mutations du WP7 comme test de mutation permanent | WP7 | moyen | moyen |
| 6.5 | `R CMD check` avec tests (`--as-cran`) et jeu d'exemples exécutables | global | élevé | moyen |

---

## 8. Ce qui n'a pas pu être vérifié, et pourquoi

| Sujet | Raison | Conséquence |
|---|---|---|
| 21 couples `(fonction, argument)` sur 456 restés en SKIP | arguments variadiques (`...`) ou « sensibles » (`file`, `path`, `output`, `open_doc`, `groq_key`) volontairement non balayés automatiquement | couverts pour l'essentiel par des cas dédiés d'EXP/SEC/PLT ; les 21 listés dans `results.csv` (verdict SKIP) restent à balayer manuellement |
| `plot_km()` — balayage de ses 4 arguments | la fonction échoue avant toute lecture d'argument (F-05) : le harnais classe l'erreur comme interne | le défaut est établi par 3 cas dédiés (PLT, BIV-KM, ALI) |
| Appels réseau réels vers Groq / Mistral | contrainte de l'audit : aucun envoi réel de données | seule la voie « sans clé » et le serveur factice (`webfakes`) ont été exercés ; le comportement HTTP réel (401/429/500, délais) n'est pas testé |
| Interface Shiny — parcours utilisateur complet | aucune automatisation de navigateur disponible dans l'environnement | seul le démarrage (port, réponse HTTP) a été vérifié ; les modules de l'application ne sont pas audités |
| `run_gui()` — comportement si `inst/app` est absent | non simulable sans altérer l'installation | message d'erreur lu dans le code, non exercé |
| Conversion LibreOffice des `.docx` en PDF | LibreOffice non disponible dans l'environnement | validité vérifiée par `officer::docx_summary()` et le XML, pas par un rendu PDF |
| Comportement sous Windows / macOS | environnement Linux uniquement | les défauts liés aux chemins, `system()`, `xdg-open`/`shell.exec` et aux locales peuvent différer |
| Performances (jeux volumineux) | hors périmètre du plan | non mesurées |
| Rendu visuel réel des graphiques | seule la constructibilité (`ggplot_build`) est vérifiée | l'esthétique et les libellés à l'écran ne sont pas jugés |

**Limites de méthode** : les oracles sont eux-mêmes des implémentations R (`stats`, `pROC`, `mice`,
`binom`, `survival`) ; une erreur commune au paquet et à son oracle ne serait pas détectée. Les
verdicts PASS signifient « conforme à l'oracle et au contrat documenté », non « statistiquement
optimal ». Enfin, l'audit juge le comportement, pas les choix de conception : la présence de 54 alias
n'est pas un défaut en soi (hypothèse 3 du plan).

---

## 9. Livrables

| Livrable | Chemin |
|---|---|
| Matrice d'audit (510 lignes) | `audit_matrix.csv` |
| Résultats complets (937 cas) | `results/results.csv` |
| Un fichier par constat (23) | `findings/F-01.md` … `findings/F-23.md` |
| Index des constats | `findings/README.md` |
| Tests de non-régression (43 tests) | `tests_nonregression/` |
| Contrôles documentaires | `results/doc_checks.rds` |
| Validation par mutation | `results/mutation_injection.rds`, `results/mutation_detection.csv` |
| Environnement (traçabilité) | `00_env/sessionInfo.txt`, `00_env/packages.txt`, `00_env/signatures.txt` |

**Suite proposée** : une seconde passe, sur votre demande, appliquant les correctifs des axes 1, 2 et 5
en s'appuyant sur `tests_nonregression/` (les tests marqués « défaut F-xx » sont conçus pour être
inversés au fur et à mesure des corrections).
