(ns datom.source-test
  (:require [clojure.test :refer [deftest is testing]]
            [datom.source :as src]
            [datom.source.conformance :as conf]))

(deftest reference-source-conforms-to-itself
  (testing "the suite is not vacuous: the reference implementation passes"
    (is (empty? (conf/check src/of-quads)) (conf/report (conf/check src/of-quads)))))

(deftest the-suite-catches-a-broken-implementation
  (testing "a source that ignores the object position is REJECTED -- otherwise
           conformance would prove nothing"
    (let [broken (fn [quads]
                   (reify src/IPatternSource
                     (-scan [_ [s p _]]
                       (src/scan-set (src/of-quads quads) [s p nil]))))
          failures (conf/check broken)]
      (is (seq failures))
      (is (some #(= :by-object (:case %)) failures))))
  (testing "a source that returns a BAG instead of a set is rejected"
    (let [dup (fn [quads]
                (reify src/IPatternSource
                  (-scan [_ pattern]
                    (concat (src/scan (src/of-quads quads) pattern)
                            [{:s "ghost" :p "ghost" :o "ghost"}]))))]
      (is (seq (conf/check dup))))))

(deftest merged-is-a-source-and-partitions-round-trip
  (let [halves (partition-all 3 conf/corpus)
        m (src/merged (map src/of-quads halves))]
    (testing "k partitions answer exactly as one whole"
      (is (empty? (conf/check (fn [quads]
                                (src/merged (map src/of-quads
                                                 (partition-all 2 quads))))))))
    (testing "and a merged source is itself mergeable (they nest)"
      (is (= (src/scan-set (src/of-quads conf/corpus) [nil "knows" nil])
             (src/scan-set (src/merged [m (src/of-quads [])]) [nil "knows" nil]))))
    (testing "a quad duplicated across partitions appears once -- which is why
             partitioned writers never have to coordinate"
      (let [dupd (src/merged [(src/of-quads conf/corpus) (src/of-quads conf/corpus)])]
        (is (= (src/scan-set (src/of-quads conf/corpus) [nil nil nil])
               (src/scan-set dupd [nil nil nil])))))))

(deftest filtered-hides-quads-from-every-pattern
  (let [no-alice (src/filtered (src/of-quads conf/corpus) #(not= "alice" (:s %)))]
    (is (empty? (src/scan-set no-alice ["alice" nil nil])))
    (is (= 2 (count (src/scan-set no-alice [nil nil nil]))))))

(deftest counting-records-the-shape-of-the-work
  (let [c (src/counting (src/of-quads conf/corpus))]
    (src/scan c [nil "knows" nil])
    (src/scan c ["alice" nil nil])
    (is (= 2 (:scans (src/counts c))))
    (is (= 6 (:quads (src/counts c))))))

(deftest pattern-helpers
  (is (= #{:s :p} (src/bound-positions ["a" "b" nil])))
  (is (= 3 (src/selectivity-hint [nil nil nil])))
  (is (= 0 (src/selectivity-hint ["a" "b" "c"]))))
