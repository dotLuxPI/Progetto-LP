%%%% Magliani Andrea 894395
%%%% Picco Nicolas 894588
%%%% Perego Luca 894448

%%%% DEF_CLASS PREDICATE
def_class(Classname, Parents) :-
    def_class(Classname, Parents, []).
def_class(Classname, Parents, Parts) :-
    catch(classname_check(Classname),
          class_already_defined, fail),
    parents_check(Parents),
    parts_check(Parts),
    concat_parts(Parts, Parents, Result),
    assertz(class(Classname, Parents, Result)).

%%%% DEF_CLASS UTILS
classname_check(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _),
    write('The class already exists!'),
    throw(class_already_defined).
classname_check(Classname) :-
    atom(Classname).

parents_check([]) :- !.
parents_check([H | T]) :-
    atom(H),
    clause(class(H, _, _), _),
    !,
    parents_check(T).

parts_check([]) :- !.
parts_check([H | T]) :-
    is_field_param(H),
    !,
    parts_check(T).
parts_check([H | T]) :-
    is_method(H),
    create_method_predicate(H),
    !,
    parts_check(T).

create_method_predicate(method(MethodName, Args, Form)) :-
    Pred =.. [MethodName, _Instance | Args],
    Clause =.. [':-', Pred, Form],
    assertz(Clause).

is_field_param(Field) :-
    Field = field(FieldName, _Value, Type),
    atom(FieldName),
    atom(Type).
is_field_param(Field) :-
    Field = field(FieldName, _Value),
    atom(FieldName).

is_method(Method) :-
    Method = method(MethodName, Args, Form),
    atom(MethodName),
    is_list(Args),
    callable(Form).

concat_parts(Parts, Parents, Result) :-
    append(Parts, [], TempResult),
    check_parents_parts(Parents, TempResult, Result).

check_parents_parts([], NewParts, NewParts) :- !.
check_parents_parts([H | T], NewParts, Result) :-
    findall(Parts, class(H, _, Parts), PartsList),
    flatten(PartsList, FlatParts),
    append_parts(NewParts, FlatParts, UpdatedNewParts),
    check_parents_parts(T, UpdatedNewParts, Result).

append_parts(NewParts, [], NewParts) :-
    !.
append_parts(NewParts, [field(Key, _Value) | Tail], Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts, [field(Key, _Value) | Tail], Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts, [field(Key, _Value, _Type) | Tail], Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts, [field(Key, _Value, _Type) | Tail], Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts, [method(Key, _, _) | Tail], Result) :-
    memberchk(method(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts, [field(Key, Value) | Tail], Result) :-
    append(NewParts, [field(Key, Value)], UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.
append_parts(NewParts, [field(Key, Value, Type) | Tail], Result) :-
    append(NewParts, [field(Key, Value, Type)], UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.
append_parts(NewParts, [method(Key, Args, Form) | Tail], Result) :-
    append(NewParts, [method(Key, Args, Form)], UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.



%%%% MAKE PREDICATE
make(InstanceName, Classname) :-
    make(InstanceName, Classname, []).
make(InstanceName, Classname, ParameterList) :-
    catch(is_valid_instancename(InstanceName),
          instance_found, fail),
    clause(class(Classname, _, _), _),
    is_list(ParameterList),
    is_parameter(ParameterList),
    concat_parameter(ParameterList, Classname, FinalParameter),
    assertz(instance(InstanceName, Classname, FinalParameter)).



%%%% MAKE UTILS
is_var(X) :-
    var(X),
    throw(var).
is_var(X) :-
    var(X).

is_valid_instancename(InstanceName) :-
    atom(InstanceName),
    clause(instance(InstanceName, _, _), _),
    write('The instance already exists!'),
    throw(instance_found).
is_valid_instancename(InstanceName) :-
    atom(InstanceName).

is_parameter([]) :- !.
is_parameter([H | T]) :-
    is_parameter_check(H),
    is_parameter(T).

is_parameter_check(Field = Value) :-
    atom(Field),
    atomic(Value).

concat_parameter(ParameterList, Classname, FinalParameter) :-
    findall(class(Classname, Parents, Parts),
            class(Classname, Parents, Parts),
            [class(Classname, Parents, Parts) | _T]),
    append_parameter(ParameterList, Parts, FinalParameter).

append_parameter(ParameterList, [], ParameterList) :-
    !.
append_parameter(ParameterList, [field(Key, _Value) | T], FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList, [field(Key, _Value, _Type) | T], FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList, [method(_Key, _Args, _Form) | T], FinalParameter) :-
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList, [field(Key, Value) | T], FinalParameter) :-
    append(ParameterList, [Key = Value], IntermediateParameter),
    append_parameter(IntermediateParameter, T, FinalParameter),
    !.
append_parameter(ParameterList, [field(Key, Value, _Type) | T], FinalParameter) :-
    append(ParameterList, [Key = Value], IntermediateParameter),
    append_parameter(IntermediateParameter, T, FinalParameter),
    !.



%%%% IS_CLASS PREDICATE
is_class(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _).



%%%% IS_INSTANCE PREDICATE
is_instance(Value) :-
    is_instance(Value, _).
is_instance(Value) :-
    clause(Value, _).
is_instance(Value, Classname) :-
    clause(instance(Value, Classname, _), _).



%%%% INST PREDICATE
inst(InstanceName, H) :-
    bagof(instance(InstanceName, Classname, ParameterList),
          instance(InstanceName, Classname, ParameterList),
          [H | _T]),
    !.
inst(_InstanceName, _Instance) :-
    write("Instance not found!"),
    fail.



%%%% FIELD PREDICATE
field(Instance, FieldName, Result) :-
    atom(Instance),
    inst(Instance, Inst),
    field(Inst, FieldName, Result).
field(instance(_InstanceName, _Classname, ParameterList), FieldName, Result) :-
    atom(FieldName),
    memberchk(FieldName = Value, ParameterList),
    Result = Value.



%%%% FIELDX PREDICATE
fieldx(Instance, FieldNames, Result) :-
    atom(Instance),
    is_list(FieldNames),
    inst(Instance, Inst),
    fieldx(Inst, FieldNames, Result).
fieldx(_Instance, [], _Result) :-
    write('The fieldnames list is empty!'),
    !.
fieldx(Instance, [H], Result) :-
    field(Instance, H, Result),
    !.
fieldx(Instance, [H | T], Result) :-
    field(Instance, H, _),
    fieldx(Instance, T, Result).


