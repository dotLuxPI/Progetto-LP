%%%% Magliani Andrea 894395
%%%% Picco Nicolas 894588
%%%% Perego Luca 894448



%%%% DYNAMIC PREDICATE DEFINITION
:- dynamic class/3.
:- dynamic instance/3.



%%%% TYPE MAP

%%%% get_type/2: return the type of Value
get_type(Value, ValueType) :-
    integer(Value),
    ValueType = 'integer',
    !.
get_type(Value, ValueType) :-
    rational(Value),
    ValueType = 'rational',
    !.
get_type(Value, ValueType) :-
    float(Value),
    ValueType = 'float',
    !.
get_type(Value, ValueType) :-
    number(Value),
    ValueType = 'number',
    !.
get_type(Value, ValueType) :-
    string(Value),
    ValueType = 'string',
    !.
get_type(Value, ValueType) :-
    atom(Value),
    ValueType = 'atom',
    !.
get_type(Value, ValueType) :-
    atomic(Value),
    ValueType = 'atomic',
    !.

%%%% integer type management
is_subtype(integer, integer).
is_subtype(integer, number).
is_subtype(integer, float).
is_subtype(integer, rational).
is_subtype(integer, atomic).
%%%% float type management
is_subtype(float, float).
is_subtype(float, number).
is_subtype(float, atomic).
%%%% rational type management
is_subtype(rational, rational).
is_subtype(rational, number).
is_subtype(rational, atomic).
%%%% number type management
is_subtype(number, number).
is_subtype(number, atomic).
%%%% string type management
is_subtype(string, string).
is_subtype(string, atom).
is_subtype(string, atomic).
%%%% atom type management
is_subtype(atom, atom).
is_subtype(atom, atomic).
%%%% atomic type management
is_subtype(atomic, atomic).



%%%% DEF_CLASS PREDICATE

%%%% def_class/2: call def_class/3
def_class(Classname, Parents) :-
    def_class(Classname, Parents, []).

%%%% def_class/3: if all check are ok
%%%% do inheritance and then assert the class
def_class(Classname, Parents, Parts) :-
    catch(classname_check(Classname),
          class_already_defined, fail),
    parents_check(Parents),
    parts_check(Parts, Classname),
    concat_parts(Parts, Parents, Classname, Result),
    assertz(class(Classname, Parents, Result)).



%%%% DEF_CLASS UTILS

%%%% classname_check/1: check if the classname is valid
%%%% and if the class isn't already defined
classname_check(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _),
    write('The class already exists!'),
    throw(class_already_defined).
classname_check(Classname) :-
    atom(Classname).


%%%% parents_check/1: check if all the parents exist
parents_check([]) :- !.
parents_check([H | T]) :-
    atom(H),
    clause(class(H, _, _), _),
    !,
    parents_check(T).


%%%% parts_check/2 call parts_check/4
parts_check(Parts, Classname) :-
    parts_check(Parts, Classname, [], []).

%%%% parts_check/4: check if the parts have a valid structure.
%%%% if all is ok create dinamically the defined methods
parts_check([], _, _, _) :- !.
parts_check([H | _T], _Classname, SeenFields, _SeenMethod) :-
    H = field(Key, _Value, _),
    memberchk(Key, SeenFields),
    write('Cannot define two field '),
    writeln('with the same name in the same class!'),
    !,
    fail.
parts_check([H | _T], _Classname, _SeenFields, SeenMethod) :-
    is_method(H, Key),
    memberchk(Key, SeenMethod),
    write('Cannot define two method '),
    writeln('with the same name in the same class!'),
    !,
    fail.
parts_check([H | T], Classname, SeenFields, SeenMethod) :-
    is_field_param(H, Key),
    !,
    parts_check(T, Classname, [Key | SeenFields], SeenMethod).
parts_check([H | T], Classname, SeenFields, SeenMethod) :-
    is_method(H, Key),
    create_method_predicate(H, Classname),
    !,
    parts_check(T, Classname, SeenFields, [Key | SeenMethod]).


%%%% create_method_predicate/2: build dinamically a predicate
%%%% with the specs passed as arguments
create_method_predicate(method(MethodName, Args, Form),
                        Classname) :-
    Pred =.. [MethodName, Instance | Args],
    Clause =.. [':-', Pred,
                (pred_check(Instance, MethodName, Classname),
                 replace_in_predicate(Instance, Form, NewForm),
                 !,
                 NewForm)],
    assertz(Clause).

%%%% pred_check/3: define all the base check that all the methods
%%%% must have
pred_check(Instance, MethodName, Classname) :-
    atom(Instance),
    is_instance(Instance),
    inst(Instance, instance(_InstanceName, Class, _ParameterList)),
    bagof(class(Class, Parents, Fields),
          class(Class, Parents, Fields),
          [class(C, _P, F) | _]),
    C = Classname,
    memberchk(method(MethodName, _, _), F).
pred_check(instance(InstanceName, Classname, ParameterList),
           MethodName) :-
    is_instance(instance(InstanceName, Classname, ParameterList)),
    bagof(class(Classname, Parents, Fields),
          class(Classname, Parents, Fields),
          [class(_C, _P, F) | _]),
    memberchk(method(MethodName, _, _), F).


%%%% replace_in_predicate/3: works with replace_this_in_args/3 and
%%%% substitute all the this keyword with the InstanceName passed as
%%%% argument
replace_in_predicate(Instance,
                     (Predicate, Rest),
                     (NewPredicate, NewRest)) :-
    Predicate =.. [Functor | Args],
    replace_this_in_args(Instance, Args, NewArgs),
    NewPredicate =.. [Functor | NewArgs],
    !,
    replace_in_predicate(Instance, Rest, NewRest).
replace_in_predicate(Instance, Predicate, NewPredicate) :-
    Predicate =.. [Functor | Args],
    replace_this_in_args(Instance, Args, NewArgs),
    NewPredicate =.. [Functor | NewArgs],
    !.

replace_this_in_args(_, [], []).
replace_this_in_args(Instance, [H | T], [NewH | NewT]) :-
    compound(H),
    replace_in_predicate(Instance, H, NewH),
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, [H | T], [H | NewT]) :-
    var(H),
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, ['this' | T], [Instance | NewT]) :-
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, [H | T], [H | NewT]) :-
    replace_this_in_args(Instance, T, NewT).


%%%% is_field_param/2: check if a field has a right definition
is_field_param(Field, Key) :-
    Field = field(FieldName, Value, Type),
    atom(FieldName),
    atom(Type),
    check_type(Value, Type),
    Key = FieldName.
is_field_param(Field, Key) :-
    Field = field(FieldName, _Value),
    atom(FieldName),
    Key = FieldName.


%%%% check_type/2: check if the Value and the type match
check_type(Value, Type) :-
    atom(Value),
    is_instance(Value, Type),
    !.
check_type(instance(InstanceName, Classname, ParameterList),
           Type) :-
    is_instance(instance(InstanceName,
                        Classname,
                        ParameterList), Type),
    !.
check_type(Value, Type) :-
    get_type(Value, ValueType),
    is_subtype(ValueType, Type),
    !.
check_type(_Value, _Type) :-
    writeln('Invalid Type or Type and Value mismatching'),
    !,
    fail.


%%%% is_method/2: check if a method has the right definition
is_method(Method, Key) :-
    Method = method(MethodName, Args, Form),
    atom(MethodName),
    is_list(Args),
    callable(Form),
    Key = MethodName.


%%%% concat_parts/4: manage the inheritance of the parts for the
%%%% def_class predicate
concat_parts(Parts, Parents, Classname, Result) :-
    check_parents_type(Parts, Parents, ModifiedParts),
    append(ModifiedParts, [], TempResult),
    check_parents_parts(Parents, TempResult, Classname, Result).


%%%% check_parents_type/3: subpredicate of concatparts for manage the
%%%% inheritance of the parents type
check_parents_type(Parts, [], Parts) :- !.
check_parents_type(Parts, [ParentsHead | ParentsTail], ModifiedParts) :-
    findall(P, class(ParentsHead, _, P), ParentsPart),
    flatten(ParentsPart, FlattenParentParts),
    check_all_parts(Parts, FlattenParentParts, ModifiedParts),
    check_parents_type(ModifiedParts, ParentsTail, ModifiedParts).

%%%% check_all_parts/3: subpredicate of concatparts for manage the
%%%% inheritance of the parts
check_all_parts([], _ParentParts, []) :-
    !.
check_all_parts([PartsHead | PartsTail], ParentParts,
                [ModifiedPartsHead | ModifiedPartsTail]) :-
    PartsHead = field(Key, _, Type),
    member(field(Key, _, ParentType), ParentParts),
    is_subtype(Type, ParentType),
    ModifiedPartsHead = field(Key, _, Type),
    !,
    check_all_parts(PartsTail, ParentParts, ModifiedPartsTail).
check_all_parts([PartsHead | PartsTail], ParentParts,
                [ModifiedPartsHead | ModifiedPartsTail]) :-
    PartsHead = field(Key, Value),
    member(field(Key, _, ParentType), ParentParts),
    check_type(Value, ParentType),
    ModifiedPartsHead = field(Key, Value, ParentType),
    !,
    check_all_parts(PartsTail, ParentParts, ModifiedPartsTail).
check_all_parts([PartsHead | PartsTail], ParentParts,
                [PartsHead | PartsTail]) :-
    PartsHead = field(Key, _Value),
    \+ member(field(Key, _, _), ParentParts),
    !,
    check_all_parts(PartsTail, ParentParts, PartsTail).
check_all_parts([PartsHead | PartsTail], ParentParts,
                [PartsHead | PartsTail]) :-
    PartsHead = field(Key, _Value, _Type),
    \+ member(field(Key, _, _), ParentParts),
    !,
    check_all_parts(PartsTail, ParentParts, PartsTail).
check_all_parts([PartsHead | PartsTail], ParentParts,
                [PartsHead | PartsTail]) :-
    PartsHead = method(_, _, _),
    !,
    check_all_parts(PartsTail, ParentParts, PartsTail).

%%%% check_parents_parts/4: subpredicate of concat parts for manage the
%%%% inheritance of the parents parts
check_parents_parts([], NewParts, _Classname, NewParts) :- !.
check_parents_parts([H | T], NewParts, Classname, Result) :-
    findall(Parts, class(H, _, Parts), PartsList),
    flatten(PartsList, FlatParts),
    append_parts(NewParts, FlatParts, Classname, UpdatedNewParts),
    check_parents_parts(T, UpdatedNewParts, Classname, Result).

%%%% append_parts/4: subpredicate of concat parts for manage the
%%%% inheritance of the parts
append_parts(NewParts, [], _Classname, NewParts) :-
    !.
append_parts(NewParts,
             [field(Key, _Value) | Tail],
             Classname,
             Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value) | Tail],
             Classname,
             Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value, _Type) | Tail],
             Classname,
             Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value, _Type) | Tail],
             Classname,
             Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [method(Key, _, _) | Tail],
             Classname,
             Result) :-
    memberchk(method(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [field(Key, Value) | Tail],
             Classname,
             Result) :-
    append(NewParts, [field(Key, Value)],
           UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [field(Key, Value, Type) | Tail],
             Classname,
             Result) :-
    append(NewParts, [field(Key, Value, Type)],
           UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Classname, Result),
    !.
append_parts(NewParts,
             [method(Key, Args, Form) | Tail],
             Classname,
             Result) :-
    append(NewParts, [method(Key, Args, Form)],
           UpdatedNewParts),
    create_method_predicate(method(Key, Args, Form), Classname),
    append_parts(UpdatedNewParts, Tail, Classname, Result),
    !.



%%%% MAKE PREDICATE
%%%% make/2: call the make/3
make(InstanceName, Classname) :-
    make(InstanceName, Classname, []).
%%%% make/3 first case: if InstanceName, Classname and
%%%% ParameterList are valid assert the instance
make(InstanceName, Classname, ParameterList) :-
    atom(Classname),
    clause(class(Classname, _, _), _),
    is_list(ParameterList),
    catch(is_valid_instancename(InstanceName),
          instance_found, fail),
    is_parameter(ParameterList),
    is_valid_parameter_list(ParameterList, Classname),
    concat_parameter(ParameterList, Classname, FinalParameter),
    assertz(instance(InstanceName, Classname, FinalParameter)),
    !.
%%%% make/3 second case: do the same check of the make first case but
%%%% instead of assert the instance create an anonymous var
make(InstanceName, Classname, ParameterList) :-
    var(InstanceName),
    atom(Classname),
    clause(class(Classname, _, _), _),
    is_list(ParameterList),
    is_parameter(ParameterList),
    is_valid_parameter_list(ParameterList, Classname),
    concat_parameter(ParameterList, Classname, FinalParameter),
    InstanceName = instance('anonymous', Classname, FinalParameter),
    !.
%%%% make/3 third case: given a make with some parameter not defined,
%%%% return the value of that parameter from the existing instance
make(InstanceName, Classname, ParameterList) :-
    bagof(instance(InstanceName, Classname, ParameterList),
          instance(InstanceName, Classname, ParameterList),
          Result),
    member(instance(InstanceName, Classname, ParameterList), Result).


%%%% MAKE UTILS

%%%% is_valid_instancename/1: check if the instancename is valid
is_valid_instancename(InstanceName) :-
    atom(InstanceName),
    clause(instance(InstanceName, _, _), _),
    write('The instance already exists!'),
    throw(instance_found).
is_valid_instancename(InstanceName) :-
    atom(InstanceName).

%%%% is_parameter/1: check if the parameter structure is valid
is_parameter([]) :- !.
is_parameter([H | T]) :-
    is_parameter_check(H),
    is_parameter(T).

is_parameter_check(Field = _Value) :-
    atom(Field).

%%%% is_valid_parameter_list/2: check if the parameter list is valid and
%%%% if the field exist in the class and if the type is matching
is_valid_parameter_list([], _) :-
    !.
is_valid_parameter_list([Field = Value | T], Classname) :-
    bagof(Parts, class(Classname, _, Parts), Parts),
    flatten(Parts, FlatParts),
    member(field(Field, _, Type), FlatParts),
    check_type(Value, Type),
    !,
    is_valid_parameter_list(T, Classname).
is_valid_parameter_list([Field = _Value | T], Classname) :-
    bagof(Parts, class(Classname, _, Parts), Parts),
    flatten(Parts, FlatParts),
    member(field(Field, _), FlatParts),
    !,
    is_valid_parameter_list(T, Classname).


%%%% concat_parameter/3: manage the inheritance from class to instance
concat_parameter(ParameterList, Classname, FinalParameter) :-
    findall(class(Classname, Parents, Parts),
            class(Classname, Parents, Parts),
            [class(Classname, Parents, Parts) | _T]),
    append_parameter(ParameterList, Parts, FinalParameter).

%%%% append_parameter/3: subpredicate of concat_parameter for manage the
%%%% inheritance from class to instance
append_parameter(ParameterList, [], ParameterList) :-
    !.
append_parameter(ParameterList,
                 [field(Key, _Value) | T],
                 FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, _Value, _Type) | T],
                 FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [method(_Key, _Args, _Form) | T],
                 FinalParameter) :-
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, Value) | T],
                 FinalParameter) :-
    append(ParameterList,
           [Key = Value],
           IntermediateParameter),
    append_parameter(IntermediateParameter,
                     T,
                     FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, Value, _Type) | T],
                 FinalParameter) :-
    append(ParameterList,
           [Key = Value],
           IntermediateParameter),
    append_parameter(IntermediateParameter,
                     T,
                     FinalParameter),
    !.



%%%% IS_CLASS PREDICATE
is_class(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _).



%%%% IS_INSTANCE PREDICATE
is_instance(Value) :-
    atom(Value),
    is_instance(Value, _).
is_instance(instance(InstanceName,
                     Classname,
                     ParameterList)) :-
    clause(instance(InstanceName,
                    Classname,
                    ParameterList), _),
    !.
is_instance(Value, Classname) :-
    atom(Value),
    clause(instance(Value, Classname, _), _),
    !.
is_instance(instance(InstanceName,
                     Classname,
                     ParameterList),
            Classname) :-
   clause(instance(InstanceName,
                   Classname,
                   ParameterList), _),
   !.
is_instance(Value, Classname) :-
    atom(Value),
    findall(Class,
            instance(Value, Class, _),
            [H | _]),
    has_parents(H, Classname),
    !.
is_instance(instance(InstanceName,
                     Class,
                     ParameterList),
            Classname) :-
    findall(Class,
            instance(InstanceName,
                     Class,
                     ParameterList),
            [H | _]),
    has_parents(H, Classname),
    !.


%%%% IS_INSTANCE UTILS

%%%% has_parents/2: check if the class has superclass as parents, or
%%%% grandparents, ecc...
has_parents(Class, SuperClass) :-
    findall(Parents, class(Class, Parents, _), P),
    flatten(P, FlatParents),
    get_parents(FlatParents, AllParents),
    memberchk(SuperClass, AllParents).

%%%% get_parents/2: get all parents, grandparents ecc..., of a given in
%%%% input class list
get_parents([], []).
get_parents([H | T], AllParents) :-
    findall(Parents, class(H, Parents, _), P),
    flatten(P, FlatP),
    get_parents(FlatP, GrandParents),
    append([H], GrandParents, TempParents),
    get_parents(T, RestParents),
    append(TempParents, RestParents, AllParents).



%%%% INST PREDICATE

%%%% inst/2: get the instance from a instance name
inst(InstanceName, H) :-
    bagof(instance(InstanceName, Classname, ParameterList),
          instance(InstanceName, Classname, ParameterList),
          [H | _T]),
    !.
inst(_InstanceName, _Instance) :-
    write("Instance not found!"),
    fail.



%%%% FIELD PREDICATE

%%%% get the value of a field in the Instance
field(Instance, FieldName, Result) :-
    atom(Instance),
    inst(Instance, Inst),
    field(Inst, FieldName, Result).
field(instance(_InstanceName, _Classname, ParameterList),
      FieldName,
      Result) :-
    atom(FieldName),
    memberchk(FieldName = Value, ParameterList),
    Result = Value.



%%%% FIELDX PREDICATE

%%%% get the value of the last field in FieldNames, can pass through
%%%% instance
fieldx(Instance, FieldNames, Result) :-
    atom(Instance),
    is_list(FieldNames),
    inst(Instance, Inst),
    fieldx(Inst, FieldNames, Result),
    !.
fieldx(_Instance, [], _Result) :-
    write('The fieldnames list is empty!'),
    !.
fieldx(Instance, [H], Result) :-
    field(Instance, H, Result),
    !.
fieldx(Instance, [H | T], Result) :-
    field(Instance, H, Res),
    is_instance(Res),
    fieldx(Res, T, Result),
    !.
fieldx(Instance, [H | T], Result) :-
    field(Instance, H, _),
    fieldx(Instance, T, Result),
    !.


