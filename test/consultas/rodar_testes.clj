(ns consultas.rodar-testes
  "Ponto de entrada de `clojure -M:test`."
  (:require [clojure.test :as t]
            [consultas.bateria-test]
            [consultas.builder-test]
            [consultas.testes]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'consultas.bateria-test
                                          'consultas.builder-test
                                          'consultas.testes)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
