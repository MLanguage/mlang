(load-file "ir_2018.clj")

(defn -main
  [& args]
  (println (get (compute_ir { "1AJ" (mk_value 10023.0) "1BJ" (mk_value 42598.0) "0CF" (mk_value 1.0) }) "v")))
