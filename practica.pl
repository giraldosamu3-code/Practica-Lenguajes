
% Esto permite que el programa agregue o elimine datos en tiempo de ejecución.
:- dynamic estudiante/4. 

% Inicio del programa
inicio :-
    cargar_desde_archivo,
    menu.

menu :-
    write('\e[36m'), 
    writeln('==#=   ==============  =============   =#=='),
    writeln('=#== SISTEMA DE REGISTRO UNIVERSITARIO ==#='),
    writeln('==#=   ==============  =============   =#==\n'),
    write('\e[0m'),
    writeln('1. | Registrar Entrada        |'),
    writeln('2. | Registrar Salida         |'),
    writeln('3. | Buscar Estudiante por ID |'),
    writeln('4. | Listar Estudiantes       |'),
    writeln('5. | Salir                    |'),
    
    write('Seleccione una opcion: '),
    read(Opcion),
    ejecutar_opcion(Opcion).

ejecutar_opcion(5) :- 
    writeln('Saliendo del sistema...'), !.

ejecutar_opcion(Opc) :-
    procesar(Opc),
    menu. 

% ingreso
procesar(1) :-
    write('Ingrese ID del estudiante: '), read(ID),
    (   estudiante(ID, adentro, _, _)
    ->  writeln('Error: El estudiante ya se encuentra en la universidad.') ;   
        write('Ingrese hora de entrada (minutos totales desde 00:00): '), read(Hora),
        retractall(estudiante(ID, _, _, _)), % Elimina registro previo si existe
        assertz(estudiante(ID, adentro, Hora, 0)),
        guardar_en_archivo,
        writeln('Entrada registrada exitosamente.')
    ).

% salida
procesar(2) :-
    write('Ingrese ID del estudiante: '), read(ID),
    (   estudiante(ID, adentro, Entrada, _)
    ->  write('Ingrese hora de salida (minutos totales desde 00:00): '), read(Salida),
        retract(estudiante(ID, adentro, Entrada, _)),
        assertz(estudiante(ID, afuera, Entrada, Salida)),
        guardar_en_archivo,
        calcular_y_mostrar_tiempo(Entrada, Salida),
        writeln('Salida registrada exitosamente.')
    ;   writeln('Error: El estudiante no esta en la universidad o el ID no existe.')
    ).

procesar(3) :-
    write('Ingrese ID a buscar: '), read(ID),
    (   estudiante(ID, Estado, Entrada, Salida)
    ->  format('Estudiante ID: ~w | Estado: ~w | Entrada: ~w min | Salida: ~w min ~n', [ID, Estado, Entrada, Salida])
    ;   write('\e[31m'), writeln('Estudiante no encontrado.')
    ).

procesar(4) :-
    write('\e[34m'), 
    writeln('=============   =============='),
    writeln('==== LISTA DE ESTUDIANTES ===='),
    writeln('=============   =============='),
    write('\e[0m'),
    forall(estudiante(ID, Estado, E, S), 
           format('ID: ~w | Estado: ~w | Entrada: ~w | Salida: ~w ~n', [ID, Estado, E, S])).

% Si la opción no es válida
procesar(_) :- writeln('Opcion no valida.').

calcular_y_mostrar_tiempo(Entrada, Salida) :-
    TiempoTotal is Salida - Entrada,
    Horas is TiempoTotal // 60,
    Minutos is TiempoTotal mod 60,
    format('Tiempo en la universidad: ~w horas y ~w minutos. ~n', [Horas, Minutos]).

cargar_desde_archivo :-
    retractall(estudiante(_, _, _, _)),
    (   exists_file('University.txt')
    ->  setup_call_cleanup(
            open('University.txt', read, Key),
            leer_datos(Key),
            close(Key)
        )
    ;   open('University.txt', write, Out), close(Out) % Crea el archivo si no existe
    ).

leer_datos(Key) :-
    read(Key, Termino),
    (   Termino == end_of_file
    ->  true
    ;   assertz(Termino),
        leer_datos(Key)
    ).

guardar_en_archivo :-
    setup_call_cleanup(
        open('University.txt', write, Key),
        (   forall(estudiante(ID, Estado, E, S), 
            format(Key, 'estudiante(~w, ~w, ~w, ~w). ~n', [ID, Estado, E, S]))
        ),
        close(Key)
    ).