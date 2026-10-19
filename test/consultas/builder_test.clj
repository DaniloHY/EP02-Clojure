(ns consultas.builder-test
  "Contrato do builder (enunciado, secao 6) e no desconhecido."
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [consultas.core :as core]
            [consultas.bateria-test :as bateria]))

(def dados bateria/dados)

(defn- titulos [registros] (mapv :titulo registros))

(t/deftest consulta-vazia
  (t/is (= {:estagios [] :terminal nil} (core/consulta)))
  (t/is (= 8 (count (core/executar (core/consulta) dados)))))

(t/deftest derivacao-sem-alterar-a-base
  (let [base   (core/consulta)
        dramas (core/onde base [:= :genero "drama"])
        dois   (core/limitar dramas 2)
        tres   (core/limitar dramas 3)]
    (t/is (= {:estagios [] :terminal nil} base))
    (t/is (= [[:onde [:= :genero "drama"]]] (:estagios dramas)))
    (t/is (= [[:onde [:= :genero "drama"]] [:limitar 2]] (:estagios dois)))
    (t/is (= [[:onde [:= :genero "drama"]] [:limitar 3]] (:estagios tres)))
    (t/is (= ["Cidade de Deus" "Central do Brasil"]
             (titulos (core/executar dois dados))))
    (t/is (= ["Cidade de Deus" "Central do Brasil" "Que Horas Ela Volta"]
             (titulos (core/executar tres dados))))
    (t/is (= 5 (count (core/executar dramas dados))))))

(t/deftest ast-do-texto-igual-a-ast-do-builder
  (t/is (= (-> (core/consulta)
               (core/onde [:= :genero "drama"])
               (core/limitar 2))
           (core/analisar "onde genero = \"drama\" | limitar 2")))
  (t/is (= (-> (core/consulta)
               (core/onde [:e [:= :genero "drama"] [:>= :nota 8]])
               (core/ordenar-por :nota :desc)
               (core/selecionar [:titulo :nota]))
           (core/analisar
            "onde genero = \"drama\" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota")))
  (t/is (= (-> (core/consulta) (core/ordenar-por :ano :asc))
           (core/analisar "ordenar por ano")))
  (t/is (= (-> (core/consulta)
               (core/onde [:ou [:= :a 1] [:e [:= :b 2] [:nao [:< :c 3.5]]]]))
           (core/analisar "onde a = 1 ou b = 2 e nao c < 3.5")))
  (t/is (= (core/contar (core/consulta)) (core/analisar "contar")))
  (t/is (= (core/agrupar-por (core/consulta) :genero [:media :duracao])
           (core/analisar "agrupar por genero com media duracao")))
  (t/is (= {:estagios [] :terminal nil} (core/analisar ""))))

(t/deftest ast-e-dado-puro
  (let [ast (core/analisar "onde nota >= 8 | ordenar por nota desc | limitar 2 | selecionar titulo")]
    (t/is (not-any? fn? (tree-seq coll? seq ast)))))

(t/deftest selecionar-tem-que-ser-o-ultimo
  (let [depois (-> (core/consulta)
                   (core/selecionar [:titulo])
                   (core/limitar 2))
        r      (core/executar depois dados)]
    (t/is (map? r))
    (t/is (seq (:erros r)))))

(t/deftest consulta-invalida-devolve-erros
  (let [r (core/executar (core/onde (core/consulta) [:= :diretor "x"]) dados)]
    (t/is (seq (:erros r)))
    (t/is (str/includes? (first (:erros r)) "diretor"))))

(t/deftest builder-nao-aceita-estagio-depois-do-terminal
  (t/is (thrown? clojure.lang.ExceptionInfo
                 (core/limitar (core/contar (core/consulta)) 2))))

(t/deftest erro-de-sintaxe-e-ex-info
  (t/is (thrown? clojure.lang.ExceptionInfo (core/analisar "voar")))
  (t/is (thrown? clojure.lang.ExceptionInfo (core/analisar "onde (ano > 2000"))))

(t/deftest no-desconhecido-falha-nomeando-o-no
  (let [falha (fn [ast]
                (try (core/executar ast dados)
                     nil
                     (catch Exception e e)))]
    (t/is (str/includes? (ex-message (falha {:estagios [[:voar 3]] :terminal nil}))
                         "voar"))
    (t/is (str/includes? (ex-message (falha {:estagios [[:onde [:xor [:= :ano 1] [:= :ano 2]]]]
                                             :terminal nil}))
                         "xor"))
    (t/is (str/includes? (ex-message (falha {:estagios [] :terminal [:minimo :ano]}))
                         "minimo"))))

(t/deftest esquema-desconhecido
  (let [outros {:esquema   [[:cidade :texto] [:habitantes :inteiro] [:area :decimal]]
                :registros [{:cidade "A" :habitantes 10 :area 1.5}
                            {:cidade "B" :habitantes 30 :area 2.5}
                            {:cidade "C" :habitantes 20 :area 0.5}]}]
    (t/is (= [{:cidade "B"}]
             (core/executar (-> (core/consulta)
                                (core/onde [:> :habitantes 25])
                                (core/selecionar [:cidade]))
                            outros)))
    (t/is (= 60 (core/executar (core/soma (core/consulta) :habitantes) outros)))
    (t/is (= 4.5 (core/executar (core/soma (core/consulta) :area) outros)))
    (t/is (= ["B" "C" "A"]
             (map :cidade (core/executar (core/ordenar-por (core/consulta) :habitantes :desc)
                                         outros))))))

(t/deftest leitor-de-csv
  (let [{:keys [esquema registros]} (core/ler-csv "a:texto;b:inteiro;c:decimal\nx;1;2\n\ny;3;4.5\n")]
    (t/is (= [[:a :texto] [:b :inteiro] [:c :decimal]] esquema))
    (t/is (= [{:a "x" :b 1 :c 2.0} {:a "y" :b 3 :c 4.5}] registros)))
  (t/is (thrown? clojure.lang.ExceptionInfo (core/ler-csv "a:foo\n1")))
  (t/is (thrown? clojure.lang.ExceptionInfo (core/ler-csv "a:inteiro\nabc")))
  (t/is (thrown? clojure.lang.ExceptionInfo (core/ler-csv "a:inteiro;b:texto\n1")))
  (t/is (thrown? clojure.lang.ExceptionInfo (core/ler-csv ""))))
