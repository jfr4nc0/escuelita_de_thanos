cree(gabriel,[campanita,mago_de_oz,cavenaghi]).
cree(juan,[conejo_de_pascuas]).
cree(macarena,[reyes_magos,mago_capria,campanita]).
cree(diego,[_]).

desea(gabriel,[ganarLoteria([5,9]),futbolista("Arsenal")]).
desea(juan,[cantante(100000)]).
desea(macarena,[cantante(10000)]).

equiposChicos(["Arsenal","Aldosivi"]).

personaAmbiciosa(Persona) :-
    desea(Persona,Deseos),
    maplist(dificultad,Deseos,Dificultades),
    sum_list(Dificultades, Total),
    Total > 20.

dificultad(cantante(Cantidad),Dificultad) :- Cantidad > 500000, !, Dificultad is 6.
dificultad(cantante(_),Dificultad) :- Dificultad is 4.
dificultad(ganarLoteria(Numeros),Dificultad) :- length(Numeros,Cantidad), Dificultad is 10 * Cantidad.
dificultad(futbolista(Equipo),Dificultad) :- equiposChicos(EquiposChicos), member(Equipo, EquiposChicos), !, Dificultad is 3.
dificultad(futbolista(_),Dificultad) :- Dificultad is 13.
