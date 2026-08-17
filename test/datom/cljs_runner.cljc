(ns datom.cljs-runner
  "Run the portable suite under a real ClojureScript host (cljs.main --target
  node).

  `datom.source` is `.cljc` and is the sole runtime dependency of
  `kotoba-lang/datalog`, but nothing in this repo had ever run it under
  ClojureScript -- there was no CI here at all. That matters more than usual
  in this fleet: the recurring defect this month has been `.cljc` whose cljs
  half is broken while the JVM half is green, and the runtime that actually
  ships is the cljs one (the object-store and D1 workers are Cloudflare
  Workers). A portability claim no machine checks is a claim.

    clojure -Sdeps '{:paths [\"src\" \"test\"]}' -M:cljs \\
      -m cljs.main --target node -m datom.cljs-runner"
  (:require [clojure.test :as t :refer [run-tests]]
            [datom.source-test]
            [datom.source.adjacency-test]
            [datom.source.plane-test]))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests] [m]
     ;; Without this a failing suite exits 0 and the gate is green forever.
     (when-not (t/successful? m)
       (set! (.-exitCode js/process) 1))))

(defn -main []
  (run-tests 'datom.source-test
             'datom.source.adjacency-test
             'datom.source.plane-test))

#?(:cljs (-main))
