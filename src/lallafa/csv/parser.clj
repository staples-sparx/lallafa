(ns lallafa.csv.parser
  "CSV (char(s) seperated values) parser that gives user fine grained control"
  (:require
   [clojure.stacktrace :as strace])
  (:import
   (java.io Closeable InputStreamReader BufferedReader FileInputStream PushbackReader)
   (com.opencsv CSVParser)))

(set! *warn-on-reflection* true)

(defn- is-line-sep-char? [line-sep c]
  (some (set line-sep) (str c)))

(defn- close [closables]
  (doall (map (fn [c] (when c (.close ^Closeable c)))
              closables)))

(defn- read-lines [file-path & [opts]]
  "Intended to be used to read in large files."
  (let [lines-to-read (atom (or (:max-lines opts) Integer/MAX_VALUE))
        fis (FileInputStream. ^String file-path)
        isr (InputStreamReader. ^java.io.FileInputStream fis
                                (or ^String (:encoding opts) "UTF-8"))
        br (PushbackReader. isr)
        line-sep (or (:end-of-line opts) "\n")
        end-of-line (StringBuilder.)
        accumulate-lines (fn [sb acc]
                           (let [s (.. ^StringBuilder sb toString trim)]
                             (if (seq s)
                               (do (swap! lines-to-read dec)
                                   (conj acc s))
                               acc)))]
    (loop [c (.read br)
           [result line-sb eol] [[] (StringBuilder.) (StringBuilder.)]]
      (if (or (= c -1) (zero? @lines-to-read))
        (do (close [fis isr br])
            (accumulate-lines line-sb result))
        (recur (.read br)
               (let [ch (char c)
                     eol (if (is-line-sep-char? line-sep ch)
                           (.append end-of-line ch)
                           end-of-line)]
                 (if (= (.toString eol) line-sep)
                   [(accumulate-lines line-sb result)
                    (doto ^StringBuilder line-sb (.setLength 0))
                    (doto ^StringBuilder eol (.setLength 0))]
                   [result
                    (->  ^StringBuilder line-sb
                         (.append ch))
                    eol])))))))

(def ^:private default-opts
  {:escape \\,
   :ignoreLeadingWhiteSpace true,
   :ignoreQuotations false,
   :pending false,
   :quotechar \",
   :separator \,,
   :strictQuotes false
   :skip-header false
   :error-handler (fn [e coll]
                    (println :kits/read-csv :malformed-csv-line coll)
                    (strace/print-stack-trace e))})

(defn- make-csv-parser [opts]
  (let [opt (merge default-opts opts)]
    (CSVParser. (:separator opt) (:quotechar opt) (:escape opt)
                (:strictQuotes opt) (:ignoreLeadingWhiteSpace opt)
                (:ignoreQuotations opt))))

(defn- parse-line [parser line error-handler]
  (when (seq line)
    (try
      (parser line)
      (catch Exception e
        (error-handler e line)
        nil))))

(defn parse [csv-file-path & [opts]]
  (let [csv-parser (make-csv-parser opts)
        line-parser (if (:multi-line opts)
                      #(.parseLineMulti ^CSVParser csv-parser %)
                      #(.parseLine ^CSVParser csv-parser %))
        read-liner (or (:read-liner opts) read-lines)
        parser (or (:line-parser opts) line-parser)
        all-lines (read-liner csv-file-path opts)
        lines (if (:skip-header opts)
                (rest all-lines)
                all-lines)]
    (loop [[h & t] lines
           parsed []]
      (if (nil? h)
        parsed
        (recur t (if-let [ph (parse-line parser
                                         h
                                         (or (:error-handler opts)
                                             (:error-handler default-opts)))]
                   (conj parsed ph)
                   parsed))))))
