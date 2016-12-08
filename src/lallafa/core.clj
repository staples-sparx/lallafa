(ns lallafa.core
  (:require
   [lallafa.csv.parser :as parser]
   [lallafa.csv.transformer :as transformer]))

(set! *warn-on-reflection* true)

(defn ->rows [csv-file-path & [opts]]
  (parser/parse csv-file-path opts))

(defn rows->coll [rows opts]
  (transformer/rows->coll rows opts))

(defn rows->map [rows opts]
  (transformer/rows->map rows opts))
