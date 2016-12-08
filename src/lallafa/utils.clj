(ns lallafa.utils
  (:require
   [clojure.pprint :as pprint]
   [clojure.data :as data])
  (:import (java.io File)))

(defn tmp-file
  "Creates an empty file in the default temporary-file directory,
   using the given prefix and suffix to generate its name.  Returns
   its path. File will be deleted on JVM exit"
  [^String prefix ^String suffix]
  (let [f (doto
            (File/createTempFile "temp",".txt")
            (.deleteOnExit))]
    (.getPath f)))

(defn tmp-file-with
  "Creates a file with provided content in the default temporary-file directory,
   using the given prefix and suffix to generate its name. Returns its
   path. File will be deleted on JVM exit"
  [^String prefix ^String suffix ^String content]
  (let [path (tmp-file prefix suffix)]
    (spit path content)
    path))

(defmacro nil-on-exceptions [& body]
  `(try
     ~@body
     (catch Exception e#
       nil)))

(defn parse-int [str]
  (nil-on-exceptions
   (Integer/parseInt str)))

(defn diff
  [expected actual]
  (if-not (= expected actual)
    (let [[only-in-expected only-in-actual, in-both] (data/diff expected actual)]
      {:only-in-expected only-in-expected
       :only-in-actual only-in-actual})
    nil))

(defn ===
  "Compare 2 values with = and print diff on stdout when they do not match. Useful for friendly test failures."
  [expected actual]
  (if-let [delta (diff expected actual)]
    (do
      (pprint/pprint delta)
      false)
    true))
