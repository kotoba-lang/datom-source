(ns datom.source
  "The seam every kotobase query path plugs into.

  kotobase's query namespaces (`arrangement.query`, `arrangement.datalog/q`)
  take a MATERIALIZED db — a map of four in-memory indices. That one choice
  fixes the cost of every query at O(database) rather than O(result), because
  producing the db means reading the whole index tree first. Measured on
  `arrangement` (2026-08-01): 57ms/50 block-reads at 2k facts, 678ms/640 at
  32k, linear in the size of the DATABASE regardless of how few rows come
  back.

  Rewriting that in place would mean deciding, once and for all, which of
  several strategies is right — cursor scans, compaction over partitions,
  precomputed views, adjacency indexes — before any of them has been measured
  against the others. This namespace exists so that decision does not have to
  be made in advance.

  A **pattern source** answers exactly one question: given `[s p o]` with nil
  for wildcard, which quads match? Everything else — how the index is stored,
  whether it is one tree or two hundred, whether the answer was precomputed —
  is behind that. Sources compose: `merged` turns k of them into one, so a
  partitioned root is just a merge of its partitions, and a compacted root is
  the same source after folding. Implementations live in whichever library
  owns the storage they read; this library owns only the contract, the
  combinators, and — the part that makes the pieces interchangeable rather
  than merely similar — the CONFORMANCE SUITE every implementation must pass.

  Deliberately zero dependencies. A seam that drags a storage engine in with
  it is not a seam."
  #?(:clj (:require [clojure.set :as set])
     :cljs (:require [clojure.set :as set])))

(defprotocol IPatternSource
  "The one operation. `pattern` is a 3-vector `[s p o]`; nil in a position is
  a wildcard. Returns a seqable of `{:s :p :o}` maps.

  Implementations MUST:
  - return every matching quad exactly once (a set, not a bag),
  - treat `[nil nil nil]` as `everything`,
  - never return a quad that contradicts the bound positions.

  Implementations MAY return quads in any order. Callers that need order
  must impose it; a source that happens to be sorted must not be relied on
  for sortedness, or swapping in another source silently changes results."
  (-scan [this pattern]))

(defn scan
  "Quads matching `pattern` in `src`. See IPatternSource."
  [src pattern]
  (-scan src pattern))

(defn scan-set
  "`scan` as a set. Convenience for callers comparing sources — which is most
  of them, since interchangeability is the point."
  [src pattern]
  (into #{} (scan src pattern)))

;; ── combinators ──────────────────────────────────────────────────────
;; Each of these is a source, so they nest. That is what makes the pieces
;; composable rather than merely pluggable.

(defn- matches? [{:keys [s p o]} [ps pp po]]
  (and (or (nil? ps) (= ps s))
       (or (nil? pp) (= pp p))
       (or (nil? po) (= po o))))

(defrecord ^:no-doc SeqSource [quads]
  IPatternSource
  (-scan [_ pattern] (into #{} (filter #(matches? % pattern)) quads)))

(defn of-quads
  "The reference source: a seq of quads, filtered per scan. O(n) per query and
  meant to be — it exists to be the thing every other implementation is
  checked AGAINST, not to be fast."
  [quads]
  (->SeqSource (vec quads)))

(defrecord ^:no-doc MergedSource [sources]
  IPatternSource
  (-scan [_ pattern]
    (reduce (fn [acc s] (into acc (scan s pattern))) #{} sources)))

(defn merged
  "One source over many. This is what a partitioned root reads through: each
  writer owns a partition, and the reader sees their union as a single plane.

  The union is a SET, so a quad asserted independently into two partitions
  appears once — the answer stays correct under duplication, only the bytes
  are wasted. That property is why partitioning does not have to coordinate."
  [sources]
  (->MergedSource (vec sources)))

(defrecord ^:no-doc FilteredSource [src pred]
  IPatternSource
  (-scan [_ pattern] (into #{} (filter pred) (scan src pattern))))

(defn filtered
  "Post-filter every quad, e.g. the `visible?` predicate `arrangement.query`
  requires. Kept as a combinator rather than a parameter of `-scan` so that an
  implementation cannot forget to apply it: wrapping is visible at the call
  site, a forgotten argument is not."
  [src pred]
  (->FilteredSource src pred))

(defrecord ^:no-doc CountingSource [src calls quads]
  IPatternSource
  (-scan [_ pattern]
    (swap! calls inc)
    (let [r (scan src pattern)]
      (swap! quads + (count r))
      r)))

(defn counting
  "Wrap a source so scans and returned-quad counts are recorded in the atoms
  it carries. Comparing architectures needs a number that is not wall-clock —
  wall-clock on a warm in-memory store measures the JVM, while scans and quads
  measure the DESIGN."
  ([src] (counting src (atom 0) (atom 0)))
  ([src calls quads] (->CountingSource src calls quads)))

(defn counts [^CountingSource src]
  {:scans @(.-calls src) :quads @(.-quads src)})

;; ── pattern helpers ──────────────────────────────────────────────────

(defn bound-positions
  "Which of `[s p o]` are bound. The shape of this is what an implementation
  dispatches on to pick an index, so it is defined once here rather than
  re-derived (differently) in each one."
  [[s p o]]
  (cond-> #{} s (conj :s) p (conj :p) o (conj :o)))

(defn selectivity-hint
  "A crude ordering key: fewer wildcards first. Query planners that want to
  run the most selective clause first can use this without every source
  having to expose statistics it may not have."
  [pattern]
  (- 3 (count (bound-positions pattern))))
