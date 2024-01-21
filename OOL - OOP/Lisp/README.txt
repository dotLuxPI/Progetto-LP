Magliani Andrea	894395
Perego Luca	894448
Picco Nicolas	894588

OOL - PRIMITIVE

1. ( def-class <class-name> <parents> <part>* )

	"def-class" si occupa di definire una classe. 
	Essa richiede in input un nome, una lista di parents e un insieme
	di parts (letti con &rest).
	La funzione si occupa di richiamare tutti i controlli da eseguire
	sui 3 componenti fondamentali: classname, parents e le parts, 
	divise in methods e fields (che per comodita vengono raggruppate
	in macro-container anche se dichiarati separatamente).
	L'insieme di questi componenti viene salvato nella hash-table
	*classes-specs* in forma di association list.



2. ( make <class-name> [<field-name> <value>]* )

	"make" è la funzione incaricata di generare le istanze di una data
	classe. Richiede in input la classe di cui deve generare un'istanza
	e un insieme di coppie [<name> <value>], tramite il quale è possibile
	cambiare i valori di default della classe. 



3. ( is-class <class-name> )

	"is-class" restituisce T se il nome passato in input compare
	nell'hash-table *classes-specs*.



4. ( is-instance <value> [<class-name>] )

	"is-instance" è una funzione che restituisce T se l'istanza data
	in input è valida e generata da una classe valida.
	La funzione ha a disposizione anche un campo opzionale in cui, se
	viene fornito un class-name, controlla che l'istanza passata
	sia una classe o sottoclasse di quel class-name.



5. ( field <instance> <field-name> )

	"field" restituisce il valore del campo di una data istanza.
	Restituisce NIL se il field non è presente nella definizione
	dell'istanza



6. ( field* <instance> <field-name>+ )

	"field*" percorre una catena di field per ottenere un valore.
	Funziona chiamando ricorsivamente field sui campi forniti in 
	input.



OOL - FUNZIONI HELPER
	
is-class: controlla che la lista di parent passata in input sia valida.

concat-all: estrae tutte la parts dalla lista che iniziano con il simbolo
	indicato e li concatena. È utilizzato con: 
	delimiter := 'fields | 'methods

preliminar-parts-check: controlla che gli elementi di parts contengano
	solamente uno dei seguenti elementi: 'fields, 'methods o liste

all-listp: controlla che la lista passata contenga solo liste

parts-check: richiama dei controlli su class-fields e class-methods.

is-valid-fields: chiamata ricorsiva di is-valid-field-structure

is-valid-field-structure: controlla la lunghezza dei field e la normalizza
	per poi passarli alla funzione is-field

get-fields: richiede la lista delle parts e restituisce tutti i field

get-methods: richiede la lista delle parts e restituisce tutti i methods 

concat-parts: concatena i fields a quelli dei parents, dando priorità ai
	primi, per poi seguire l'ordine di inserimento nella dichiarazione
	dei parents.

field-validation: chiamata ricorsiva di fill-in-field

fill-in-field: analizza la struttura del field passato in input e richiama
	determinate funzioni per ereditare componenti mancanti e 
	normalizzare la lunghezza di ogni field salvato alla versione da 
	3 componenti.

manage-instance-type: metodo speciale per la gestione del tipo delle
	istanze, che altrimenti genererebbe errori sui controlli dei tipi
	nativi di Common Lisp.

fill-value-field: funzione che si occupa di gestire i casi in cui il field
	fornito è disposto solo di nome. Assegna un valore ereditato se 
	disponibile.

fill-type-field: funzione che si occupa di gestire i casi in cui il field
	fornito è disposto di nome e tipo. Controlla se il valore assegnato
	è compatibile con quello di eventuali parent-field, e in caso lo
	eredita.

compatiblity-check: funzione che effettua ulteriori controlli sul tipo
	e sulla compatibilità dei componenti con i parents.
	Gestisce alcuni casi non coperti dalle funzioni precedenti, tra
	cui i controlli sui field "completi".

get-all-fields: restituisce tutti i field, compresi quelli ereditati e 
	rimuovendo i doppioni.

rewrite-field: effettua evaluation di un valore passato in input se si 
	tratta di un istanza (o una variabile contenente la stessa).

is-valid-methods: chiamata ricorsiva di is-valid-method-structure.

is-valid-method-structure: controlla la struttura del metodo in input per 
	poi passarlo alla funzione is-method.

is-method: controlla se i componenti del metodo rispettano le specifiche
	richieste.

is-sexp: controlla che il valore passato i input sia una symbolic-expression

get-all-methods: restituisce tutti i methods, compresi quelli ereditati e
	rimuovendo i doppioni.

remove-keyword: restituisce la lista di methods in input sostituendo 
	eventuali keyword impostate come method-name.

process-method: chiama do-processing, sostituendo le keyword se passate in
	input come method-name.

get-method: restituisce un metodo dato il nome.

is-method-listed: restituisce T se il method dato input compare nella 
	lista passata come secondo argomento.

get-method-listed: restituisce il method dato input se compare nella lista
	passata come secondo argomento.

process-all-methods: chiamata ricorsiva a process-method.

make-default-instance: crea un istanza utilizzando solo i valori di default 
	della classe.

make-custom-instance: crea un istanza utilizzando i valori dati in input
	alla chiamata della primitiva make. Completa i campi non inseriti
	dall'utente con i valori di default della classe.

is-valid-instance-value: controlla che i valori esterni passati per la 
	creazione di un'istanza personalizzata siano compatibili con i tipi
	della classe.

inherit-field: restituisce tutti i field di una classe seguendo la forma
	utilizzata nelle istanze ((name-1 value-1) ... (name-n value-n)).

is-name-present: restituisce T se esiste un field con il nome dato in input
	nella lista passata come argomento.

is-subclass-of: restituisce T se il primo argomento della funzione è una 
	sottoclasse del secondo.

get-all-parents: restituisce una lista contenente tutti i parents della 
	classe identificata dal class-name dato in input.
	