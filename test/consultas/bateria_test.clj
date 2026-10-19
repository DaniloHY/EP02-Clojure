(ns consultas.bateria-test
  "A bateria casos_teste_ep02.txt, transcrita como dado. Usa o mesmo caminho
  do programa (analisar -> executar -> formatar) via saida/responder."
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [consultas.core :as core]
            [consultas.main :as main]
            [consultas.saida :as saida]))

(def csv-filmes
  (str "titulo:texto;ano:inteiro;genero:texto;nota:decimal;duracao:inteiro\n"
       "Cidade de Deus;2002;drama;8.6;130\n"
       "Central do Brasil;1998;drama;8.0;113\n"
       "O Auto da Compadecida;2000;comedia;8.6;104\n"
       "Tropa de Elite;2007;acao;8.0;115\n"
       "Bacurau;2019;suspense;7.4;131\n"
       "Que Horas Ela Volta;2015;drama;7.7;112\n"
       "Aquarius;2016;drama;7.5;145\n"
       "Ainda Estou Aqui;2024;drama;8.2;137\n"))

(def dados (core/ler-csv csv-filmes))

(defn- resp [consulta] (saida/responder dados consulta))

(defn- l [& linhas] (str/join "\n" linhas))

(def casos-ok
  [;; projecao e corte
   ["contar" "8"]
   ["" (l "Cidade de Deus; 2002; drama; 8.60; 130"
          "Central do Brasil; 1998; drama; 8.00; 113"
          "O Auto da Compadecida; 2000; comedia; 8.60; 104"
          "Tropa de Elite; 2007; acao; 8.00; 115"
          "Bacurau; 2019; suspense; 7.40; 131"
          "Que Horas Ela Volta; 2015; drama; 7.70; 112"
          "Aquarius; 2016; drama; 7.50; 145"
          "Ainda Estou Aqui; 2024; drama; 8.20; 137")]
   ["limitar 3 | selecionar titulo"
    (l "Cidade de Deus" "Central do Brasil" "O Auto da Compadecida")]
   ["onde titulo = \"Aquarius\"" "Aquarius; 2016; drama; 7.50; 145"]
   ["onde titulo = \"Cidade de Deus\" | selecionar ano, titulo"
    "2002; Cidade de Deus"]

   ;; ordenacao estavel
   ["ordenar por ano | limitar 3 | selecionar titulo, ano"
    (l "Central do Brasil; 1998" "O Auto da Compadecida; 2000" "Cidade de Deus; 2002")]
   ["ordenar por nota desc | selecionar titulo, nota"
    (l "Cidade de Deus; 8.60" "O Auto da Compadecida; 8.60" "Ainda Estou Aqui; 8.20"
       "Central do Brasil; 8.00" "Tropa de Elite; 8.00" "Que Horas Ela Volta; 7.70"
       "Aquarius; 7.50" "Bacurau; 7.40")]
   ["ordenar por nota | selecionar titulo, nota"
    (l "Bacurau; 7.40" "Aquarius; 7.50" "Que Horas Ela Volta; 7.70"
       "Central do Brasil; 8.00" "Tropa de Elite; 8.00" "Ainda Estou Aqui; 8.20"
       "Cidade de Deus; 8.60" "O Auto da Compadecida; 8.60")]

   ;; expressoes
   ["onde (genero = \"acao\" ou genero = \"suspense\") e nao ano < 2010 | selecionar titulo"
    "Bacurau"]
   ["onde genero = \"acao\" ou genero = \"suspense\" e ano < 2010 | selecionar titulo"
    "Tropa de Elite"]
   ["onde nota = 8 | selecionar titulo" (l "Central do Brasil" "Tropa de Elite")]
   ["onde nota != 8.6 | contar" "6"]
   ["onde nao nota >= 8 | selecionar titulo"
    (l "Bacurau" "Que Horas Ela Volta" "Aquarius")]

   ;; terminais
   ["soma duracao" "987"]
   ["soma nota" "64.00"]
   ["media nota" "8.00"]
   ["agrupar por genero com media duracao"
    (l "acao; 115.00" "comedia; 104.00" "drama; 127.40" "suspense; 131.00")]
   ["agrupar por genero com soma duracao"
    (l "acao; 115" "comedia; 104" "drama; 637" "suspense; 131")]
   ["agrupar por nota com contar"
    (l "7.40; 1" "7.50; 1" "7.70; 1" "8.00; 2" "8.20; 1" "8.60; 2")]
   ["onde ano >= 2010 | agrupar por genero com contar"
    (l "drama; 3" "suspense; 1")]

   ;; exemplos do enunciado
   ["agrupar por genero com contar"
    (l "acao; 1" "comedia; 1" "drama; 5" "suspense; 1")]
   ["onde nota < 7.5 | contar" "1"]
   ["onde ano > 2010 | ordenar por ano | limitar 2 | selecionar titulo, ano"
    (l "Que Horas Ela Volta; 2015" "Aquarius; 2016")]
   ["onde genero = \"drama\" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota"
    (l "Cidade de Deus; 8.60" "Ainda Estou Aqui; 8.20" "Central do Brasil; 8.00")]
   ["onde genero = \"drama\" | media duracao" "127.40"]

   ;; resultados vazios
   ["onde ano > 3000 | contar" "0"]
   ["onde ano > 3000 | soma duracao" "0"]
   ["onde ano > 3000 | media nota" "0.00"]
   ["onde ano > 3000" "(vazio)"]
   ["onde ano > 3000 | agrupar por genero com contar" "(vazio)"]])

(def casos-erro
  [;; esquema
   "onde diretor = \"x\" ou estudio = \"y\""
   "selecionar titulo, pais, idioma"
   "onde ano = \"x\" e pais = \"BR\" | soma titulo"
   "onde ano = \"2002\""
   ;; sintaxe
   "|"
   "voar"
   "onde titulo contem \"de\""
   "pular 2"
   "limitar abc"
   "contar | limitar 2"
   "selecionar titulo | onde ano > 2000"
   "selecionar titulo | contar"
   "minimo ano"
   "onde (ano > 2000"
   "ordenar ano"
   "agrupar por genero"
   "onde ano >"
   ;; extras
   "onde ano > 2000 |"
   "limitar -1"
   "onde \"abc"
   "onde ano ? 3"])

(t/deftest respostas-da-bateria
  (run! (fn [[consulta esperado]]
          (t/is (= esperado (resp consulta)) consulta))
        casos-ok))

(t/deftest erros-da-bateria
  (run! (fn [consulta]
          (t/is (str/starts-with? (resp consulta) "ERRO:") consulta))
        casos-erro))

(t/deftest erros-que-citam-o-campo
  (t/is (= "ERRO: campo inexistente: diretor" (resp "ordenar por diretor")))
  (t/is (str/includes? (resp "soma titulo") "titulo"))
  (t/is (= "ERRO: campo inexistente: diretor" (resp "onde diretor = \"x\"")))
  (t/is (= "ERRO: comparacao entre inteiro e texto no campo ano"
           (resp "onde ano = \"2002\""))))

(t/deftest comandos-do-terminal
  (t/is (= [true "8"] (main/processar dados "QUERY contar")))
  (t/is (= [false nil] (main/processar dados "EXIT")))
  (t/is (str/starts-with? (second (main/processar dados "")) "ERRO:"))
  (t/is (str/starts-with? (second (main/processar dados "   ")) "ERRO:"))
  (t/is (str/starts-with? (second (main/processar dados "query contar")) "ERRO:"))
  (t/is (str/starts-with? (second (main/processar dados "COMANDO_INEXISTENTE")) "ERRO:"))
  (t/is (= 8 (count (str/split-lines (second (main/processar dados "QUERY"))))))
  (t/is (= [true "8"] (main/processar dados "QUERY contar"))))

(t/deftest aninhamento-profundo-nao-derruba
  (let [fundo (str (apply str (repeat 20000 "(")) "ano > 1")]
    (t/is (str/starts-with? (resp (str "onde " fundo)) "ERRO:"))))
