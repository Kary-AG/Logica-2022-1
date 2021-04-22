---
title: 'Lógica Computacional 2020-II'
subtitle: 'Ejercicio Semanal 1'
author: 'Karyme I. Azpeitia García'
---

1. Lea el artículo *On the unusual effectiveness of logic in computer science* que se encuentra en la sección de material del classroom y la página del curso y entregue un comentario personal al respecto, de mínimo una cuartilla, que incluya cuál de las aplicaciones de la lógica (secciones 2 a 6) le parece más atractiva y por qué.

Se puede decir que el texto *On the unusual effectiveness of logic in computer science* muestra cómo es que la lógica está muy presente en grandes áreas de las ciencias de la computación; desarrollando la idea de que las ciencias naturales se han visto beneficiadas de las distintas ramas de las matemáticas aumentando la eficacia de ellas permitiendo así una mayor evolución, específicamente en el texto se habla de la lógica como una  de las ramas que permite establecerlas de manera completa y formalizarlas. 
Actualmente la lógica es un area de investigacion madura y con resultados profundos teniendo así un gran número de aplicaciones en determinadas áreas; por ejemplo la informática, donde la lógica ha resultado ser significativamente efectiva, por lo que se le ha llamado “el cálculo de las ciencias de la computación”.

Una de las aplicaciones mencionadas en el artículo es “Razonamiento sobre el conocimiento”; un tema que ha sido estudiado por la comunidad filosófica, denotandolo como "la lógica epistémica”. Buscando así, dar una representación de la inteligencia humana, teniendo un importante papel en diversos campos como la computación distribuida y la inteligencia artificial dado la relación entre conocimiento y acción que estas tienen.

Lo interesante de “la lógica epistémica” dentro de las ciencias de la computación es como la intuición se formaliza utilizando la lógica modal con las estructuras de Kripke enfocadas a los sistemas informáticos.

Para ejemplificar la aplicación del  “Razonamiento sobre el conocimiento” en particular en el marco de protocolos distribuidos, el artículo presenta  el problema del ataque  coordinado,que resume un problema de gestión de recuperación de datos  que surge cuando  se utiliza protocolos estándar en la gestión de base de datos  llamados  protocolos de confirmación, el problema consiste en dos divisiones de un ejército A y B, ambas en dos colinas que dominan un valle donde se encuentra el enemigo, su objetivo es atacar al mismo tiempo, para esto tienen un mensajero que con el riesgo de ser capturado por el enemigo  envía los mensajes de confirmación para el ataque, donde con la lógica, en términos de conocimientos  cada que el mensajero hace un tránsito, el conocimiento de cada división aumenta en uno. Cada que se envía un mensaje se va  formando una proposición primitiva, de esta manera el conocimiento va aumentado. Para poder formalizar esto  se toma un sistema donde se analiza tiempo, proceso y ejecución de los mensajes. Por medio de Teoremas y Corolarios el artículo muestra como se puede utilizar este enfoque para verificar, analizar y razonar sobre protocolos distribuidos.


Ejemplos como el anterior es la manera en que el  artículo nos menciona como  la lógica abarca un amplio espectro en distintas áreas como la inteligencia artificial hasta la ingeniería de software, nos menciona como la eficacia de la lógica en la información quizás no sea misteriosa ni razonable, pero si bastante notable e inusual.

2. Formalice el siguiente argumento con lógica proposicional, una vez hecha la especificación formal decida si el argumento es correcto o no, indicando claramente el método utilizado. Defina previamente el glosario, es decir, el significado de las variables empleadas en la especificación formal.

\begin{center}

\textit{Si el programa es eficiente, se ejecuta rápidamente. El programa es eficiente, o tiene un error. Sin embargo, el programa no se ejecuta rápidamente. Por lo tanto tiene un error.}

\end{center}

*Glosario*

$p::=$ el  programa es eficiente.

 
$q::=$ el programa se ejecuta rápidamente.

$e::=$ el programa tiene un error.

Lo cuál nos lleva a

*Especificación formal*

\begin{center}

$p\rightarrow q$

$(p\vee e)$

$(\neg q)$

$\rule{20mm}{0.1mm}$

$\therefore e$
\end{center}

Veamos $\{p\rightarrow q,(p\vee e),(\neg q)\}\models e$ es un argumento correcto.

*Demostración*


Mostraremos que $e$ se sigue lógicamente de $\Gamma=\{p\rightarrow q,(p\vee e),(\neg q)\}$, es decir que para toda interpretación $\mathcal{I}$ tal que $\mathcal{I}(\Gamma)=1$ entonces $\mathcal{I}(e)=1$.


Supongamos $\mathcal{I}$ tal que $\mathcal{I}(\Gamma)=1$, entonces 

$\mathcal{I}(p\rightarrow q)=1$           **(1)**

$\mathcal{I}((p\vee e) = 1$               **(2)**

$\mathcal{I}(\neg q)=1$                             **(3)**

Analizando las suposiciones anteriores, de **(3)** podemos saber que $\mathcal{I}(\neg q)=1$ es decir $\mathcal{I}(q)=0$ **(4)**

Utilizando **(4)** y siguiendo la definición  de interpretación para **(1)** podemos asignar $\mathcal{I}(p)=0$ **(5)**.

Siguiendo la definición de interpretación para **(2)** $\mathcal{I}(p\vee e)=1$ utilizando **(5)** podemos concluir $\mathcal{I}(e)=1$.

De lo anterior constrimos una interpretación tal que $e$ es consecuencia lógica de $\Gamma$. 

Por tanto el argumento es correcto.
