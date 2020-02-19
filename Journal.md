# Pensées sur Speculoos 

## (le 19/02/20) 

J’ai quelque regrets avec speculoos: 

La sensation de refaire en moins bien ce que j’avais fait dans [asparagus](https://github.com/pbaille/asparagus). Le [tutorial](https://github.com/pbaille/asparagus/blob/master/tutorial.org) met bien en avant toutes les features 

Laisser beaucoup de choses qui me semblent essentielles de coté: 
- Les mécaniques extensibles de destructuration et de macro expansion
- La destructuration d’asparagus et aussi expressive que core.match au moins (et aussi performante dans la majeure partie des cas)
- Certaines conventions très porteuse en terme composition comme celle proposant de passer toujours l’objet de la transformation en premier argument à une fonction 
- L’implementation des fonctions générique sur laquelle est construite asparagus 
- La fine surcouche au dessus du système de type de Clojure et clojurescript permettant un interface unifiée
- Les bindings modaux (symboles préfixé par ? Ou ! Indiquant le comportement en cas de binding à nil )
- Ne pas avoir le temps de repenser quelque chose d’analogue à clojure.spec mais plus élégant et moins complexe (spec mélange coercion validation et génération)
- Un système de module plus expressif (amené partiellement via la macro speculoos.utils/dof) 
- Un outil d’introspection de l’environment (pas encore implémenté)

Grace à la macro dof, asparagus va être portable en clojurescript de toute évidence 
La library [boot](https://github.com/pbaille/boot) contient les bases d’asparagus dans une version compatible clojurescript (notamment la surcouche type évoquée et l’implémentation des fonctions génériques)
En lui ajoutant ‘dof il pourrait être possible de porter asparagus en cljs 

Je pense que si l’on veut avoir un vrai impact sur la communauté il faut viser haut, et que spéculons ne va pas assez loin
Je ressens une urgence à communiquer sur le projet (alors qu’il n’a pas été assez mis à l’épreuve en interne) qui me met légèrement mal à l’aise de dois l’avouer 

Je me dis aussi que peut être que le rôle premier de spéculons est de servir parkaViz, c’est du moins dans cette optique que je l’ai mis sur pied initialement
En discutant avec Jon j’ai ressenti qu’il y voyait quelque chose que j’ai tendance à davantage voir dans asparagus (une vrai surcouche à visée générale au dessus de clojure) 
Asparagus pourrait être remanié de manière à se fondre plus facilement dans clojure sous la forme d’une simple collection de macros 

La grande majorité des briques existent et demande simplement à être bien rassemblé pour un minimum de friction 
Le tutorial, contient déjà une grande quantité d’exemple et documente de manière substantielle la majeure partie des features d’asparagus 
En quelques semaines de travail nous pourrions arrivé à en faire une réalité, peut être pas quelque chose que le développeur lambda puisse utiliser en deux clics mais après tout clojurescript est resté assez obscure et intouchable pour le profane pendant bien longtemps, donc peut être que la priorité n’est pas là dans un premier temps
Je crois qu’il faut nous réserver le temps de l’utiliser et de le tester sérieusement avant de le publier au large.

Voila ce que je pense aujourd’hui et en vitesse 

Je ne suis pas entrain de dire que speculoos en tant que tel n’est pas montrable ou quoi que ce soit, ni que ma motivation pour l’utiliser dans le cadre de parkaViz à baissé, c’est juste qu’en grossissant il m’invite au parallèle avec asparagus et certaines question émergent. 