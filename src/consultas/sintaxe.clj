(ns consultas.sintaxe
  "Analise sintatica: texto da consulta -> AST.

  A AST e dado puro (mapas, vetores, palavras-chave, numeros e strings).
  Cada funcao de analise recebe a sequencia de tokens que falta ler e
  devolve um par [no tokens-restantes]; nenhuma variavel guarda a posicao
  de leitura. Erros de sintaxe sao sinalizados com ex-info.")

(defn- erro [mensagem]
  (throw (ex-info mensagem {:tipo :sintaxe})))

;; ---------------------------------------------------------------------------
;; Tokens
;;
;; Um token e um vetor:
;;   [:ident "nome"] [:num 8] [:num 8.6] [:str "texto"] [:op :>=]
;;   [:abre] [:fecha] [:virgula] [:barra]
;; ---------------------------------------------------------------------------

;; Todo caractere que nao e espaco casa com alguma alternativa (a ultima,
;; \S, pega o que sobrar), entao nada e descartado em silencio.
(def ^:private padrao-token
  #"\"[^\"]*\"|-?\d+(?:\.\d+)?|[\p{L}_][\p{L}\p{N}_-]*|!=|<=|>=|[=<>(),|]|\S")

(def ^:private operadores #{"=" "!=" "<" "<=" ">" ">="})

(defn- inteiro [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException _
      (erro (str "inteiro fora do limite: " s)))))

(defn- classificar [s]
  (cond
    (and (> (count s) 1) (= \" (first s)) (= \" (last s)))
    [:str (subs s 1 (dec (count s)))]

    (re-matches #"-?\d+" s)               [:num (inteiro s)]
    (re-matches #"-?\d+\.\d+" s)          [:num (Double/parseDouble s)]
    (re-matches #"[\p{L}_][\p{L}\p{N}_-]*" s) [:ident s]
    (contains? operadores s)              [:op (keyword s)]
    (= s "(")                             [:abre]
    (= s ")")                             [:fecha]
    (= s ",")                             [:virgula]
    (= s "|")                             [:barra]
    :else (erro (str "caractere inesperado: " s))))

(defn tokenizar
  "Texto -> vetor de tokens."
  [texto]
  (mapv classificar (re-seq padrao-token (str texto))))

(defn- descrever [[tipo valor]]
  (case tipo
    :ident valor
    :str   (str "\"" valor "\"")
    :num   (str valor)
    :op    (name valor)
    :abre  "("
    :fecha ")"
    :virgula ","
    :barra "|"
    "fim da consulta"))

;; ---------------------------------------------------------------------------
;; Expressoes: ou < e < nao < parenteses / comparacao
;; ---------------------------------------------------------------------------

(declare expr-ou)

(defn- comparacao [[campo operador literal & resto]]
  (cond
    (not= :ident (first campo))
    (erro (str "campo esperado, encontrado " (descrever campo)))

    (not= :op (first operador))
    (erro (str "operador esperado depois do campo " (second campo)
               ", encontrado " (descrever operador)))

    (nil? literal)
    (erro (str "falta o literal depois de " (name (second operador))))

    (not (contains? #{:num :str} (first literal)))
    (erro (str "literal esperado, encontrado " (descrever literal)))

    :else
    [[(second operador) (keyword (second campo)) (second literal)] resto]))

(defn- primario [[tok & resto :as tokens]]
  (cond
    (nil? tok)
    (erro "expressao incompleta: falta uma condicao")

    (= [:abre] tok)
    (let [[no depois] (expr-ou resto)]
      (if (= [:fecha] (first depois))
        [no (rest depois)]
        (erro "parentese sem fechar")))

    :else
    (comparacao tokens)))

(defn- expr-nao [[tok & resto :as tokens]]
  (if (= [:ident "nao"] tok)
    (let [[no depois] (expr-nao resto)]
      [[:nao no] depois])
    (primario tokens)))

(defn- expr-e [tokens]
  (let [[primeiro resto] (expr-nao tokens)]
    (loop [esq primeiro
           tokens resto]
      (if (= [:ident "e"] (first tokens))
        (let [[dir depois] (expr-nao (rest tokens))]
          (recur [:e esq dir] depois))
        [esq tokens]))))

(defn- expr-ou [tokens]
  (let [[primeiro resto] (expr-e tokens)]
    (loop [esq primeiro
           tokens resto]
      (if (= [:ident "ou"] (first tokens))
        (let [[dir depois] (expr-e (rest tokens))]
          (recur [:ou esq dir] depois))
        [esq tokens]))))

;; ---------------------------------------------------------------------------
;; Elementos da consulta
;; ---------------------------------------------------------------------------

(defn- elemento-ordenar [[por campo & mais]]
  (cond
    (not= [:ident "por"] por)
    (erro "falta \"por\" depois de ordenar")

    (not= :ident (first campo))
    (erro "campo esperado depois de \"ordenar por\"")

    (= [:ident "desc"] (first mais))
    [[:ordenar-por (keyword (second campo)) :desc] (rest mais)]

    :else
    [[:ordenar-por (keyword (second campo)) :asc] mais]))

(defn- elemento-limitar [[tok & mais]]
  (if (and (= :num (first tok))
           (integer? (second tok))
           (not (neg? (second tok))))
    [[:limitar (second tok)] mais]
    (erro "limitar exige um numero inteiro nao negativo")))

(defn- elemento-selecionar [tokens]
  (loop [tokens tokens
         campos []]
    (let [[tok & resto] tokens]
      (if (= :ident (first tok))
        (let [campos (conj campos (keyword (second tok)))]
          (if (= [:virgula] (first resto))
            (recur (rest resto) campos)
            [[:selecionar campos] resto]))
        (erro "campo esperado em selecionar")))))

(defn- agregacao
  "contar | soma <campo> | media <campo>  ->  [[:contar]] etc."
  [[tok & resto]]
  (case (when (= :ident (first tok)) (second tok))
    "contar" [[:contar] resto]

    ("soma" "media")
    (let [[campo & depois] resto]
      (if (= :ident (first campo))
        [[(keyword (second tok)) (keyword (second campo))] depois]
        (erro (str (second tok) " exige um campo"))))

    (erro "agregacao esperada: contar, soma <campo> ou media <campo>")))

(defn- elemento-agrupar [[por campo com & mais]]
  (cond
    (not= [:ident "por"] por)
    (erro "falta \"por\" depois de agrupar")

    (not= :ident (first campo))
    (erro "campo esperado depois de \"agrupar por\"")

    (not= [:ident "com"] com)
    (erro "falta \"com <agregacao>\" em agrupar por")

    :else
    (let [[agg resto] (agregacao mais)]
      [[:agrupar-por (keyword (second campo)) agg] resto])))

(defn- elemento [[tok & resto :as tokens]]
  (cond
    (or (nil? tok) (= [:barra] tok))
    (erro "estagio vazio")

    (not= :ident (first tok))
    (erro (str "estagio esperado, encontrado " (descrever tok)))

    :else
    (case (second tok)
      "onde"       (let [[expr depois] (expr-ou resto)]
                     [[:onde expr] depois])
      "ordenar"    (elemento-ordenar resto)
      "limitar"    (elemento-limitar resto)
      "selecionar" (elemento-selecionar resto)
      ("contar" "soma" "media") (agregacao tokens)
      "agrupar"    (elemento-agrupar resto)
      (erro (str "estagio desconhecido: " (second tok))))))

(def ^:private terminais #{:contar :soma :media :agrupar-por})

(defn- analisar-elementos [tokens estagios]
  (let [[no resto] (elemento tokens)
        tipo (first no)
        ultimo? (empty? resto)]
    (cond
      (contains? terminais tipo)
      (if ultimo?
        {:estagios estagios :terminal no}
        (erro (str "o terminal " (name tipo) " tem que ser o ultimo da consulta")))

      (= :selecionar tipo)
      (if ultimo?
        {:estagios (conj estagios no) :terminal nil}
        (erro "selecionar tem que ser o ultimo da consulta"))

      ultimo?
      {:estagios (conj estagios no) :terminal nil}

      (= [:barra] (first resto))
      (recur (rest resto) (conj estagios no))

      :else
      (erro (str "token inesperado: " (descrever (first resto)))))))

(defn analisar
  "Texto de uma consulta (sem a palavra QUERY) -> AST:
     {:estagios [estagio ...] :terminal nil-ou-no}
  Texto vazio e a consulta sem nenhum estagio."
  [texto]
  (let [tokens (tokenizar texto)]
    (if (empty? tokens)
      {:estagios [] :terminal nil}
      (analisar-elementos tokens []))))
