Wilfried
BEN BRAHIM
21000754

Compilateur C vers MIPS en OCaml

Bienvenue dans l'univers captivant de l'informatique et de la programmation, où l'ingéniosité se fusionne avec la puissance du langage machine. Notre projet novateur a donné naissance à un compilateur avant-gardiste, conçu pour traduire le langage C en langage machine MIPS. Cela ouvre la porte à une exécution efficace sur des architectures MIPS spécifiques. Cette prouesse technique a été réalisée en utilisant le langage de programmation OCaml, renforçant ainsi la robustesse et la flexibilité de notre compilateur. Au cours de cette aventure intellectuelle, nous avons exploré les profondeurs du langage C, navigué à travers les subtilités du langage machine MIPS, et exploité la puissance expressive d'OCaml pour donner vie à un outil puissant et fonctionnel. Plongeons ensemble dans ce périple fascinant qui a façonné un compilateur novateur en C avec un print et input similaire a python.
Instructions d'utilisation

Pour compiler le projet, exécutez la commande suivante :

dune build main.exe

Pour utiliser le compilateur, exécutez la commande suivante, en remplaçant ./tests/_nomdutest__.test par le chemin du fichier à compiler :

./main.exe ./tests/_nomdutest__.test > code.s

Pour exécuter le code généré, utilisez la commande suivante :

spim -f code.s

Langage pris en charge

Le compilateur prend en charge le langage C, en héritant de sa syntaxe riche et puissante. Les fonctionnalités implémentées comprennent :

    Types : string, void, bool, int, Func_t (fonctions)
    Variables
    Appels de fonctions
    Conditions
    Boucles
    Instructions de retour
    Définition et utilisation de fonctions
    Entrées et sorties standard (input et print)
    Opérateurs et opérateurs logiques

Le programme gere les erreur de types pour les appels de fonctions et de ces arguments et des variables
Il gère automatiquement le print et input en fonctions du types
Il gère les variables et fonctions nommées deux fois et les variables unbound
Le programme ne gère pas encore la récursivité

Gestion des erreurs

Le programme gère les erreurs de types pour les appels de fonctions, les arguments de fonctions et les variables. Il effectue automatiquement la gestion des instructions print et input en fonction des types. De plus, il détecte les variables et fonctions déclarées deux fois ainsi que les variables non déclarées. Cependant, la récursivité n'est pas encore prise en charge.
Démos

Découvrez les démos dans le dossier "tests" avec les fichiers suivants :

    plusoumoinsgrand.test
    pierrefeuilleciseaux.test
    jeudallumette.test
    fibonnaci.test
    
Il y a après pleins de fichiers de tests dans mon dossier "tests" afin d'y tester le bon fonctionnement du compilateur et de
ces implémentations.

Explorez et expérimentez avec ces démos pour mieux comprendre les capacités de notre compilateur C vers MIPS en OCaml. Merci de faire partie de cette passionnante aventure !

Fonctionnalités à venir

Voici quelques fonctionnalités à venir que j'aurais aimer implémenter :

    Réplication de fonctions : L'ajout de la possibilité de définir des fonctions récursives pour étendre la portée du compilateur.

    Optimisations du code généré : Nous explorons des techniques d'optimisation du code machine généré pour améliorer les performances.

    Support de bibliothèques externes : Intégration à des bibliothèques externes pour accéder à des fonctionnalités étendues sans avoir à les implémenter manuellement.

    Compatibilité avec d'autres architectures : Élargissement de la compatibilité pour permettre la génération de code pour différentes architectures.
    

Prérequis

Avant de compiler et utiliser notre compilateur C vers MIPS en OCaml, assurez-vous que vous disposez des éléments suivants installés sur votre système :

    OCaml : (assurez-vous d'avoir la version minimale requise)
    Dune : (assurez-vous d'avoir la version minimale requise)
    SPIM : Un simulateur de machine MIPS pour exécuter le code généré

Assurez-vous également d'avoir configuré vos variables d'environnement correctement pour accéder aux commandes OCaml et Dune depuis votre terminal.

Je vous remercie de votre lecture et de l'attention que vous porter pour ce projets, vos cours on été vraiment très enrichissant et permis de progresser apportement de précieuse compétence en informatique et vos cours on été très pédagogique face a un 
sujet très complexe qui est la compilation.

Wilfried
BEN BRAHIM
21000754
