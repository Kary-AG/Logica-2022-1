/*
 * | Lógica Computacional 2022-01
 * | Práctica 5: Algoritmo Hao Wang
 * | Profesor: Dr. Favio E. Miranda Perea
 * | Ayudante: Javier Enríquez Mendoza
 * | Ayudante: Ramón Arenas Ayala
 * | Laboratorio: Daniel Lugo Cano
 *
 * | Integrantes:
 * | Shin Ui Chul
 * | Azpeitia García Karyme Ivette
 * | Sánchez Reza Neider
 */

/* LÓGICA PROPOSICIONAL */

% |1| predicado que indica si dos proposiciones son equivalentes.
form_equiv(not(A), not(B)) :- form_equiv(A,B), !.
form_equiv(and(P1, Q1), and(P2, Q2)) :- form_equiv(P1, P2), form_equiv(Q1, Q2), !.
form_equiv(or(P1, Q1), or(P2, Q2)) :- form_equiv(P1, P2), form_equiv(Q1, Q2), !.
form_equiv(imp(P1, Q1), or(not(P2), Q2)) :- form_equiv(P1,P2), form_equiv(Q1,Q2), !.
form_equiv(syss(P1, Q1), and(I1, I2)) :- form_equiv(P1, P2),
					 form_equiv(Q1, Q2),
					 I1 = or(not(P2), Q2),
					 I2 = or(not(Q2), P2), !.
form_equiv(A, B) :- A = B.

/* ALGORITMO HAO WANG */

% |2| predicado que es verdadero si el elemento se encuentra en la lista.
elem(X, [A|_]) :- X = A.
elem(X, [_|L]) :- elem(X, L).

% |3| predicado que es verdadero si la intersección entre listas es no vacía.
intersect([X|_], L) :- elem(X, L), !.
intersect([_|Xs], L) :- intersect(Xs, L).

% |4| predicado que corresponde con la función eliminar de una lista.
delete(X, [Y|Ys], NL) :- X = Y, Ys = NL, !.
delete(X, [Y|Ys], [Y|NL]) :- delete(X, Ys, NL).
