(ns consultas.testes
  "Testes do grupo. Execucao: clojure -M:teste"
  (:require [clojure.test :as t]
            [consultas.core :as c]
            [consultas.saida :as saida]))

(def csv
  (str "titulo:texto;ano:inteiro;genero:texto;nota:decimal;duracao:inteiro\n"
       "Cidade de Deus;2002;drama;8.6;130\n"
       "Central do Brasil;1998;drama;8.0;113\n"
       "O Auto da Compadecida;2000;comedia;8.6;104\n"
       "Tropa de Elite;2007;acao;8.0;115\n"
       "Bacurau;2019;suspense;7.4;131\n"
       "Que Horas Ela Volta;2015;drama;7.7;112\n"
       "Aquarius;2016;drama;7.5;145\n"
       "Ainda Estou Aqui;2024;drama;8.2;137\n"))

(def dados (c/ler-csv csv))

(defn- rodar [texto]
  (let [ast (c/analisar texto)]
    (saida/formatar-resultado ast (:esquema dados) (c/executar ast dados))))

(t/deftest leitura-do-csv
  (t/is (= [[:titulo :texto] [:ano :inteiro] [:genero :texto]
            [:nota :decimal] [:duracao :inteiro]]
           (:esquema dados)))
  (t/is (= 8 (count (:registros dados))))
  (t/is (= {:titulo "Bacurau" :ano 2019 :genero "suspense" :nota 7.4 :duracao 131}
           (nth (:registros dados) 4))))

(t/deftest builder-nao-altera-a-consulta-de-partida
  (let [base   (c/consulta)
        dramas (c/onde base [:= :genero "drama"])
        dois   (c/limitar dramas 2)
        tres   (c/limitar dramas 3)]
    (t/is (= {:estagios [] :terminal nil} base))
    (t/is (= 1 (count (:estagios dramas))))
    (t/is (= ["Cidade de Deus" "Central do Brasil"]
             (mapv :titulo (c/executar dois dados))))
    (t/is (= 3 (count (c/executar tres dados))))))

(t/deftest texto-e-builder-geram-a-mesma-ast
  (t/is (= (-> (c/consulta)
               (c/onde [:= :genero "drama"])
               (c/limitar 2))
           (c/analisar "onde genero = \"drama\" | limitar 2")))
  (t/is (= {:estagios [[:onde [:e [:= :genero "drama"] [:>= :nota 8]]]
                       [:ordenar-por :nota :desc]
                       [:selecionar [:titulo :nota]]]
            :terminal nil}
           (c/analisar "onde genero = \"drama\" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota"))))

(t/deftest precedencia
  (t/is (= [:ou [:= :genero "acao"] [:e [:= :genero "suspense"] [:< :ano 2010]]]
           (second (first (:estagios (c/analisar "onde genero = \"acao\" ou genero = \"suspense\" e ano < 2010"))))))
  (t/is (= "Bacurau"
           (rodar "onde (genero = \"acao\" ou genero = \"suspense\") e nao ano < 2010 | selecionar titulo")))
  (t/is (= "Tropa de Elite"
           (rodar "onde genero = \"acao\" ou genero = \"suspense\" e ano < 2010 | selecionar titulo"))))

(t/deftest ordenacao-estavel
  (t/is (= "Cidade de Deus; 8.60\nO Auto da Compadecida; 8.60"
           (rodar "ordenar por nota desc | limitar 2 | selecionar titulo, nota")))
  (t/is (= "Central do Brasil; 8.00\nTropa de Elite; 8.00"
           (rodar "onde nota = 8 | ordenar por nota | selecionar titulo, nota"))))

(t/deftest terminais
  (t/is (= "8" (rodar "contar")))
  (t/is (= "987" (rodar "soma duracao")))
  (t/is (= "64.00" (rodar "soma nota")))
  (t/is (= "8.00" (rodar "media nota")))
  (t/is (= "6" (rodar "onde nota != 8.6 | contar")))
  (t/is (= "acao; 115.00\ncomedia; 104.00\ndrama; 127.40\nsuspense; 131.00"
           (rodar "agrupar por genero com media duracao")))
  (t/is (= "7.40; 1\n7.50; 1\n7.70; 1\n8.00; 2\n8.20; 1\n8.60; 2"
           (rodar "agrupar por nota com contar"))))

(t/deftest resultados-vazios
  (t/is (= "0" (rodar "onde ano > 3000 | contar")))
  (t/is (= "0" (rodar "onde ano > 3000 | soma duracao")))
  (t/is (= "0.00" (rodar "onde ano > 3000 | media nota")))
  (t/is (= "(vazio)" (rodar "onde ano > 3000")))
  (t/is (= "(vazio)" (rodar "onde ano > 3000 | agrupar por genero com contar"))))

(defn- erros [texto]
  (:erros (c/executar (c/analisar texto) dados)))

(t/deftest consulta-invalida-devolve-erros
  (t/is (re-find #"diretor" (first (erros "onde diretor = \"x\" ou estudio = \"y\""))))
  (t/is (re-find #"diretor" (first (erros "ordenar por diretor"))))
  (t/is (re-find #"titulo" (first (erros "soma titulo"))))
  (t/is (seq (erros "onde ano = \"x\" e pais = \"BR\" | soma titulo")))
  (t/is (= ["comparacao entre inteiro e texto no campo ano"]
           (erros "onde ano = \"2002\"")))
  ;; selecionar seguido de estagio, montado pelo builder: invalido, nao excecao
  (t/is (seq (:erros (c/executar (-> (c/consulta)
                                     (c/selecionar [:titulo])
                                     (c/limitar 2))
                                 dados)))))

(defn- erro-de-sintaxe? [texto]
  (try
    (c/analisar texto)
    false
    (catch clojure.lang.ExceptionInfo _
      true)))

(t/deftest erros-de-sintaxe-sao-ex-info
  (t/is (every? erro-de-sintaxe?
                ["|" "voar" "onde titulo contem \"de\"" "pular 2" "limitar abc"
                 "contar | limitar 2" "selecionar titulo | onde ano > 2000"
                 "selecionar titulo | contar" "minimo ano" "onde (ano > 2000"
                 "ordenar ano" "agrupar por genero" "onde ano >"
                 "limitar 2 |" "onde ano > 1 )" "onde titulo = \"aberta"])))

(t/deftest no-desconhecido-falha-nomeando-o-no
  (let [falha (fn [ast]
                (try
                  (c/executar ast dados)
                  nil
                  (catch Exception e e)))]
    (t/is (re-find #"voar" (ex-message (falha {:estagios [[:voar 3]] :terminal nil}))))
    (t/is (re-find #"xor" (ex-message (falha {:estagios [[:onde [:xor [:= :ano 1] [:= :ano 2]]]]
                                              :terminal nil}))))
    (t/is (re-find #"minimo" (ex-message (falha {:estagios [] :terminal [:minimo :ano]}))))))

(defn -main [& _]
  (let [resultado (t/run-tests 'consultas.testes)]
    (System/exit (if (t/successful? resultado) 0 1))))
