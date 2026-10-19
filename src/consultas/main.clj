(ns consultas.main
  "Namespace de entrada: unico com I/O (ler arquivo, ler o terminal, imprimir).
  O laco do terminal e recursao (loop/recur); nao ha estado fora da chamada."
  (:require [clojure.string :as str]
            [consultas.core :as core]
            [consultas.saida :as saida]))

(defn processar
  "Uma linha digitada -> [continuar? saida]. `saida` e uma string a imprimir
  ou nil. Puro: nao imprime nada."
  [dados linha]
  (let [texto (str/trim linha)]
    (if (str/blank? texto)
      [true "ERRO: linha em branco"]
      (let [[comando resto] (str/split texto #"\s+" 2)]
        (case comando
          "QUERY" [true (saida/responder dados (or resto ""))]
          "EXIT"  (if (nil? resto)
                    [false nil]
                    [true "ERRO: EXIT nao aceita argumentos"])
          [true (str "ERRO: comando desconhecido: " comando)])))))

(defn- laco
  "Imprime o prompt, le uma linha, responde e volta ao comeco. O fim da
  entrada equivale a EXIT."
  [dados]
  (loop []
    (print "> ")
    (flush)
    (when-let [linha (read-line)]
      (let [[continuar? saida] (processar dados linha)]
        (when (some? saida)
          (println saida))
        (when continuar?
          (recur))))))

(defn- carregar [caminho]
  (core/ler-csv (slurp caminho :encoding "UTF-8")))

(defn -main [& args]
  (let [caminho (first args)]
    (if (nil? caminho)
      (do (.println System/err "uso: clojure -M:run <arquivo.csv>")
          (System/exit 2))
      (let [dados (try
                    (carregar caminho)
                    (catch Exception e
                      (.println System/err
                                (str "ERRO: nao foi possivel carregar " caminho
                                     ": " (ex-message e)))
                      nil))]
        (if (nil? dados)
          (System/exit 1)
          (laco dados))))))
