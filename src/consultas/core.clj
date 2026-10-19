(ns consultas.core
  "API do motor: leitor de CSV, query builder, analisar e executar.

  Uma consulta e dado puro:
    {:estagios [[:onde expr] [:ordenar-por :campo :asc] [:limitar 3] ...]
     :terminal nil}
  As funcoes do builder recebem uma consulta e devolvem outra; a de partida
  nunca e alterada."
  (:require [clojure.string :as str]
            [consultas.sintaxe :as sintaxe]
            [consultas.compilador :as compilador]))

;; ---------------------------------------------------------------------------
;; CSV: texto -> {:esquema [[campo tipo] ...] :registros [mapa ...]}
;; ---------------------------------------------------------------------------

(def ^:private tipos-de-campo
  {"inteiro" :inteiro "decimal" :decimal "texto" :texto})

(defn- erro-csv [mensagem]
  (throw (ex-info mensagem {:tipo :csv})))

(defn- declaracao-de-campo
  "\"nome:tipo\" -> [:nome :tipo]"
  [declaracao]
  (let [partes (str/split declaracao #":" -1)
        nome   (str/trim (first partes))
        tipo   (get tipos-de-campo (str/trim (str (second partes))))]
    (cond
      (not= 2 (count partes))
      (erro-csv (str "cabecalho invalido (esperado nome:tipo): " declaracao))

      (str/blank? nome)
      (erro-csv (str "cabecalho invalido (campo sem nome): " declaracao))

      (nil? tipo)
      (erro-csv (str "tipo desconhecido no cabecalho: " (second partes)))

      :else
      [(keyword nome) tipo])))

(defn- converter [tipo texto]
  (case tipo
    :texto   texto
    :inteiro (Long/parseLong (str/trim texto))
    :decimal (Double/parseDouble (str/trim texto))))

(defn- ler-registro [esquema numero linha]
  (let [valores (str/split linha #";" -1)]
    (when (not= (count valores) (count esquema))
      (erro-csv (str "linha " numero ": esperado " (count esquema)
                     " campos, encontrado " (count valores))))
    (try
      (zipmap (map first esquema)
              (map (fn [[_ tipo] valor] (converter tipo valor)) esquema valores))
      (catch NumberFormatException e
        (erro-csv (str "linha " numero ": valor numerico invalido ("
                       (ex-message e) ")"))))))

(defn ler-csv
  "Texto do CSV -> {:esquema [[campo tipo] ...] :registros [...]}.
  A primeira linha declara nome:tipo de cada campo; o separador e `;`.
  Cada registro e um mapa de palavra-chave para valor (Long, Double ou
  String). Linhas em branco sao ignoradas."
  [texto]
  (let [texto  (str texto)
        texto  (if (str/starts-with? texto "\uFEFF") (subs texto 1) texto)
        linhas (->> (str/split-lines texto)
                    (map-indexed (fn [i linha] [(inc i) linha]))
                    (remove (fn [[_ linha]] (str/blank? linha))))]
    (if (empty? linhas)
      (erro-csv "arquivo CSV vazio: falta a linha de cabecalho")
      (let [[[_ cabecalho] & corpo] linhas
            esquema (mapv declaracao-de-campo (str/split cabecalho #";" -1))]
        (when-not (apply distinct? (map first esquema))
          (erro-csv "campo repetido no cabecalho"))
        {:esquema   esquema
         :registros (mapv (fn [[numero linha]] (ler-registro esquema numero linha))
                          corpo)}))))

;; ---------------------------------------------------------------------------
;; Builder: consulta -> consulta
;; ---------------------------------------------------------------------------

(defn consulta
  "Consulta nova, sem nenhum estagio: devolve todos os registros."
  []
  {:estagios [] :terminal nil})

(defn- acrescentar-estagio [c estagio]
  (if (some? (:terminal c))
    (throw (ex-info (str "nao cabe estagio depois do terminal: " (pr-str estagio))
                    {:tipo :construcao}))
    (update c :estagios (fnil conj []) estagio)))

(defn- definir-terminal [c terminal]
  (if (some? (:terminal c))
    (throw (ex-info (str "a consulta ja tem terminal: " (pr-str (:terminal c)))
                    {:tipo :construcao}))
    (assoc c :terminal terminal)))

(defn onde
  "Acrescenta um filtro. expr: [:= :campo valor], [:e a b], [:ou a b],
  [:nao a]; operadores := :!= :< :<= :> :>=."
  [c expr]
  (acrescentar-estagio c [:onde expr]))

(defn selecionar
  "campos: vetor de palavras-chave. Tem que ser o ultimo elemento: estagio ou
  terminal acrescentado depois dele torna a consulta invalida (executar
  devolve {:erros [...]})."
  [c campos]
  (acrescentar-estagio c [:selecionar (vec campos)]))

(defn ordenar-por
  "ordem: :asc ou :desc."
  [c campo ordem]
  (acrescentar-estagio c [:ordenar-por campo ordem]))

(defn limitar [c n]
  (acrescentar-estagio c [:limitar n]))

;; Terminais (alem do contrato minimo): fecham a consulta.

(defn contar [c]
  (definir-terminal c [:contar]))

(defn soma [c campo]
  (definir-terminal c [:soma campo]))

(defn media [c campo]
  (definir-terminal c [:media campo]))

(defn agrupar-por
  "agregacao: [:contar], [:soma :campo] ou [:media :campo]."
  [c campo agregacao]
  (definir-terminal c [:agrupar-por campo agregacao]))

;; ---------------------------------------------------------------------------
;; Texto e execucao
;; ---------------------------------------------------------------------------

(defn analisar
  "Texto de uma consulta, sem a palavra QUERY -> AST. Erro de sintaxe e
  sinalizado com ex-info."
  [texto]
  (sintaxe/analisar texto))

(defn executar
  "Executa a consulta `c` sobre `dados` ({:esquema [[campo tipo] ...]
  :registros seq}). Devolve:
    - a seq de registros (mapas), quando nao ha terminal;
    - o valor do terminal (numero), para contar, soma e media;
    - uma seq de {:chave k :valor v} em ordem crescente de chave, para
      agrupar por;
    - {:erros [...]} quando a consulta e invalida contra o esquema (nenhum
      registro e processado).
  Um no de tipo inexistente na AST faz a execucao FALHAR (excecao que nomeia
  o no); ele nao vira {:erros ...}."
  [c dados]
  (let [esquema (:esquema dados)
        erros   (compilador/conferir esquema c)]
    (if (seq erros)
      {:erros erros}
      ((compilador/compilar esquema c) (:registros dados)))))
