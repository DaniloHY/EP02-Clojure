(ns consultas.compilador
  "Duas passagens sobre a AST, ambas despachando por `case` sobre o tipo do
  no, SEM ramo padrao:

    1. conferir  - AST + esquema -> vetor de mensagens de erro (vazio = valida)
    2. compilar  - AST + esquema -> UMA funcao de seq de registros em resultado

  Um no cujo tipo nao existe na linguagem faz o `case` falhar com
  IllegalArgumentException nomeando o tipo (ex.: \"No matching clause: :voar\").
  Esse erro NAO e capturado aqui: ele sobe ate quem chamou.")

;; ---------------------------------------------------------------------------
;; 1. Conferencia contra o esquema
;; ---------------------------------------------------------------------------

(defn- nome [campo]
  (if (keyword? campo) (name campo) (str campo)))

(defn- tipo-literal [valor]
  (cond
    (string? valor)  :texto
    (integer? valor) :inteiro
    (float? valor)   :decimal))

(defn- numerico? [tipo]
  (contains? #{:inteiro :decimal} tipo))

(defn- erros-campo [tipos campo]
  (if (contains? tipos campo)
    []
    [(str "campo inexistente: " (nome campo))]))

(defn- erros-comparacao [tipos campo literal]
  (let [tipo-campo (get tipos campo)
        tipo-lit   (tipo-literal literal)]
    (cond
      (nil? tipo-campo)
      [(str "campo inexistente: " (nome campo))]

      (nil? tipo-lit)
      [(str "literal invalido na comparacao com o campo " (nome campo)
            ": " (pr-str literal))]

      (= (numerico? tipo-campo) (numerico? tipo-lit))
      []

      :else
      [(str "comparacao entre " (name tipo-campo) " e " (name tipo-lit)
            " no campo " (nome campo))])))

(defn- erros-expr [tipos [tipo & args]]
  (case tipo
    (:e :ou) (into [] (mapcat (partial erros-expr tipos)) args)
    :nao     (erros-expr tipos (first args))
    (:= :!= :< :<= :> :>=) (erros-comparacao tipos (first args) (second args))))

(defn- erros-numerico [tipos operacao campo]
  (let [tipo (get tipos campo)]
    (cond
      (nil? tipo)      [(str "campo inexistente: " (nome campo))]
      (numerico? tipo) []
      :else [(str operacao " exige campo numerico, mas " (nome campo)
                  " e " (name tipo))])))

(defn- erros-agregacao [tipos [tipo & args]]
  (case tipo
    :contar []
    (:soma :media) (erros-numerico tipos (name tipo) (first args))))

(defn- erros-terminal [tipos [tipo & args :as terminal]]
  (case tipo
    (:contar :soma :media) (erros-agregacao tipos terminal)
    :agrupar-por (into (erros-campo tipos (first args))
                       (erros-agregacao tipos (second args)))))

(defn- erros-limite [n]
  (if (and (integer? n) (not (neg? n)))
    []
    [(str "limitar exige inteiro nao negativo: " (pr-str n))]))

(defn- erros-ordem [ordem]
  (if (contains? #{:asc :desc} ordem)
    []
    [(str "ordem invalida: " (pr-str ordem) " (use :asc ou :desc)")]))

(defn- erros-estagio [tipos [tipo & args]]
  (case tipo
    :onde        (erros-expr tipos (first args))
    :ordenar-por (into (erros-campo tipos (first args))
                       (erros-ordem (second args)))
    :limitar     (erros-limite (first args))
    :selecionar  (let [campos (first args)]
                   (if (empty? campos)
                     ["selecionar exige ao menos um campo"]
                     (into [] (mapcat (partial erros-campo tipos)) campos)))))

(defn- erros-estrutura
  "selecionar so pode ser o ultimo elemento: nem estagio depois dele,
  nem terminal."
  [estagios terminal]
  (let [ultimo   (dec (count estagios))
        posicoes (keep-indexed (fn [i [tipo]] (when (= :selecionar tipo) i))
                               estagios)]
    (if (every? #(and (= % ultimo) (nil? terminal)) posicoes)
      []
      ["selecionar tem que ser o ultimo elemento da consulta"])))

(defn conferir
  "Confere a AST contra o esquema ([[campo tipo] ...]). Devolve um vetor de
  mensagens de erro; vazio quando a consulta e valida."
  [esquema ast]
  (let [tipos    (into {} esquema)
        estagios (:estagios ast)
        terminal (:terminal ast)]
    (vec (concat (erros-estrutura estagios terminal)
                 (mapcat (partial erros-estagio tipos) estagios)
                 (when terminal (erros-terminal tipos terminal))))))

;; ---------------------------------------------------------------------------
;; 2. Compilacao: AST -> funcoes
;; ---------------------------------------------------------------------------

(defn- relacao
  "Operador -> predicado sobre o resultado de `compare` (negativo, zero,
  positivo). `compare` ja trata inteiro e decimal entre si (8 e 8.0 empatam)."
  [operador]
  (case operador
    :=  zero?
    :!= (complement zero?)
    :<  neg?
    :<= (complement pos?)
    :>  pos?
    :>= (complement neg?)))

(defn- comparar-campo [teste campo literal registro]
  (teste (compare (get registro campo) literal)))

(defn- compilar-expr
  "Expressao -> predicado sobre um registro."
  [[tipo & args]]
  (case tipo
    :e   (apply every-pred (mapv compilar-expr args))
    :ou  (apply some-fn (mapv compilar-expr args))
    :nao (complement (compilar-expr (first args)))
    (:= :!= :< :<= :> :>=)
    (partial comparar-campo (relacao tipo) (first args) (second args))))

(defn- compilar-estagio
  "Estagio -> funcao de seq de registros em seq de registros. O argumento do
  estagio ja fica fixado (aplicacao parcial)."
  [[tipo & args]]
  (case tipo
    :onde        (partial filter (compilar-expr (first args)))
    :ordenar-por (let [[campo ordem] args]
                   ;; sort-by e estavel; no desc inverte-se o comparador, nao
                   ;; o resultado, para os empatados manterem a ordem de entrada.
                   (partial sort-by campo
                            (case ordem
                              :asc  compare
                              :desc (fn [a b] (compare b a)))))
    :limitar     (partial take (first args))
    :selecionar  (let [campos (first args)]
                   (partial map #(select-keys % campos)))))

(defn- compilar-agregacao
  "Agregacao -> funcao de seq de registros em valor. Sempre uma dobra."
  [tipos [tipo & args]]
  (case tipo
    :contar (partial reduce (fn [n _] (inc n)) 0)

    :soma   (let [campo (first args)
                  zero  (if (= :decimal (get tipos campo)) 0.0 0)]
              (partial reduce
                       (fn [total registro] (+' total (get registro campo)))
                       zero))

    :media  (let [campo (first args)]
              (fn [registros]
                (let [[total n] (reduce (fn [[total n] registro]
                                          [(+ total (get registro campo)) (inc n)])
                                        [0.0 0]
                                        registros)]
                  (if (zero? n) 0.0 (/ total n)))))))

(defn- compilar-terminal
  "Terminal -> funcao de seq de registros em resultado final. O resultado de
  agrupar por e uma seq de {:chave k :valor v}, em ordem crescente de chave."
  [tipos [tipo & args :as terminal]]
  (case tipo
    (:contar :soma :media) (compilar-agregacao tipos terminal)

    :agrupar-por
    (let [campo   (first args)
          agregar (compilar-agregacao tipos (second args))]
      (fn [registros]
        (->> (group-by campo registros)
             (sort-by key)
             (map (fn [[chave grupo]] {:chave chave :valor (agregar grupo)})))))))

(defn compilar
  "AST + esquema -> funcao unica (seq de registros -> resultado). A AST e
  percorrida aqui, uma vez; depois so as funcoes montadas rodam. A composicao
  e feita de modo que o primeiro estagio do texto seja o primeiro a rodar."
  [esquema ast]
  (let [tipos    (into {} esquema)
        terminal (:terminal ast)
        funcoes  (cond-> (mapv compilar-estagio (:estagios ast))
                   terminal (conj (compilar-terminal tipos terminal)))]
    (reduce (fn [f g] (comp g f)) identity funcoes)))
