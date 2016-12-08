(ns lallafa.csv.transformer
  "transforms csv fields into coll ")

(set! *warn-on-reflection* true)

(defn- row->value [csv-row {:keys [val-fn exclude-columns]
                            :or {val-fn identity}
                            :as field-reader-opts}]
  (let [add-column (fn [m i]
                     (let [col (get csv-row i)
                           field (get field-reader-opts i)]
                       (if (and field
                                (not (contains? exclude-columns i)))
                         (assoc m (:label field) ((:reader field) col))
                         m)))]
    (->> csv-row
         count
         range
         (reduce add-column nil)
         val-fn)))

(defn rows->coll [csv-rows {:keys [pred-fn]
                            :or {pred-fn (constantly true)}
                            :as field-reader-opts}]
  (->> csv-rows
       (map #(row->value % field-reader-opts))
       (filter pred-fn)))

(defn rows->map [csv-rows {:keys [key-fn]
                           :or {key-fn identity}
                           :as field-reader-opts}]
  (reduce #(assoc %1 (key-fn %2) %2)
          nil
          (rows->coll csv-rows field-reader-opts)))
