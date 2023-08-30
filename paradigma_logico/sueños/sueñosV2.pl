cree(gabriel,campanita).
cree(gabriel,mago_de_oz).
cree(gabriel,cavenaghi).
cree(juan,conejo_de_pascuas).
cree(macarena,reyes_magos).
cree(macarena,mago_capria).
cree(macarena,campanita).

desea(gabriel,ganarLoteria([5,9])).
desea(gabriel,futbolista(arsenal)).
desea(juan,cantante(100000)).
desea(macarena,cantante(100000)).

equipoChico(arsenal).
equipoChico(aldosivi).

enfermo(campanita).
enfermo(conejo_de_pascuas).

amistad(campanita,reyes_magos).
amistad(campanita,conejo_de_pascuas).
amistad(conejo_de_pascuas,cavenaghi).

persona(Persona) :- distinct(Persona,cree(Persona,_)).

calcularDificultad(Persona,Dificultad) :-
    desea(Persona,Deseo),
    dificultad(Deseo,Dificultad).

personaAmbiciosa(Persona) :-
    persona(Persona),
    aggregate_all(sum(Dificultad), calcularDificultad(Persona,Dificultad), TotalDificultad),
    TotalDificultad > 20.

dificultad(cantante(Cantidad),Dificultad) :- Cantidad > 500000, Dificultad is 6.
dificultad(cantante(Cantidad),Dificultad) :- Cantidad =< 500000, Dificultad is 4.
dificultad(ganarLoteria(Numeros),Dificultad) :- length(Numeros,Cantidad), Dificultad is 10 * Cantidad.
dificultad(futbolista(Equipo),Dificultad) :- equipoChico(Equipo), Dificultad is 3.
dificultad(futbolista(Equipo),Dificultad) :- not(equipoChico(Equipo)), Dificultad is 16.

tieneQuimica(Persona,campanita) :-
    cree(Persona,campanita),
    aggregate_all(count,(calcularDificultad(Persona,Dificultad), Dificultad < 5),Cantidad),
    Cantidad >= 1.
    
tieneQuimica(Persona,Personaje) :-
    cree(Persona,Personaje),
    forall(desea(Persona,Deseo), deseoPuro(Deseo)),
    not(personaAmbiciosa(Persona)).

deseoPuro(futbolista(_)).
deseoPuro(cantante(Cantidad)) :- Cantidad < 200000.

puedeAlegrar(Personaje,Persona) :-
    aggregate_all(count,desea(Persona,_),Cant), Cant >= 1,
    tieneQuimica(Persona,Personaje),
    algunEnfermo(Personaje).

algunEnfermo(Personaje) :- not(enfermo(Personaje)).
algunEnfermo(Personaje) :- aggregate_all(count, ,Cant), Cant >= 1. % buscar al menos uno que no este enfermo

obtenerAmistades(Personaje,AmistadesPosibles) :-
    findall(Amigo,amistad(Personaje,Amigo),Amigos),
    obtenerAmistadesDe(Amigos,AmistadesPosibles).

obtenerAmistadesDe([],[]).
obtenerAmistadesDe([Amigo|_],[Amigo|Posibles]) :-
    findall(AmigoIndirecto,amistad(Amigo,AmigoIndirecto),AmigosIndirectos),
    obtenerAmistadesDe(AmigosIndirectos,Posibles).
obtenerAmistadesDe([_|Demas],Posibles) :- obtenerAmistadesDe(Demas,Posibles).

