(ns consultas.saida
  "Formatacao do resultado e resposta a um texto de consulta. Tudo aqui e
  puro: recebe dados e devolve string; quem imprime e consultas.main."
  (:require [clojure.string :as str]
            [consultas.core :as core]))

(defn formatar-valor
  "Inteiro sai como inteiro; decimal com duas casas e ponto, em qualquer
  localidade do sistema (8.60, nunca 8,60)."
  [valor]
  (if (float? valor)
    (String/format java.util.Locale/ROOT "%.2f" (to-array [valor]))
    (str valor)))

(defn- campos-da-saida
  "Ordem das colunas: a do selecionar ou, sem ele, a do esquema. (Os
  registros sao mapas, e mapas grandes nao guardam ordem.)"
  [ast esquema]
  (if-let [estagio (first (filter #(= :selecionar (first %)) (:estagios ast)))]
    (second estagio)
    (mapv first esquema)))

(defn- formatar-registro [campos registro]
  (str/join "; " (map #(formatar-valor (get registro %)) campos)))

(defn- juntar-linhas [linhas]
  (if (empty? linhas)
    "(vazio)"
    (str/join "\n" linhas)))

(defn formatar-resultado
  "AST + esquema + resultado de executar -> texto, uma linha por registro ou
  grupo; `(vazio)` quando nao ha nenhuma."
  [ast esquema resultado]
  (case (first (:terminal ast))
    nil          (juntar-linhas
                  (map (partial formatar-registro (campos-da-saida ast esquema))
                       resultado))
    :agrupar-por (juntar-linhas
                  (map #(str (formatar-valor (:chave %)) "; "
                             (formatar-valor (:valor %)))
                       resultado))
    (formatar-valor resultado)))

(defn responder
  "Texto de uma consulta (sem a palavra QUERY) -> texto da resposta. Nunca
  lanca: qualquer problema vira uma string que comeca por `ERRO:`."
  [dados texto]
  (try
    (let [ast       (core/analisar texto)
          resultado (core/executar ast dados)]
      (if (and (map? resultado) (contains? resultado :erros))
        (str "ERRO: " (first (:erros resultado)))
        (formatar-resultado ast (:esquema dados) resultado)))
    (catch clojure.lang.ExceptionInfo e
      (str "ERRO: " (ex-message e)))
    (catch StackOverflowError _
      "ERRO: consulta aninhada demais")
    (catch Exception e
      (str "ERRO: erro interno: " (or (ex-message e) (.getName (class e)))))))
