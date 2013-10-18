(ns clojure.tools.metadata-test
  (:refer-clojure :exclude [read *default-data-reader-fn*])
  (:use [clojure.tools.reader :only [read *default-data-reader-fn*]]
        [clojure.test :only [deftest is]])
  (:require [clojure.tools.reader.reader-types :as reader-types]
            [clojure.walk :as walk])
  (:import java.nio.charset.Charset
           (java.io StringReader)
           clojure.lang.LineNumberingPushbackReader))

(def test-contents
  "Contents of a file stream for testing."
  "(ns clojure.tools.reader.haiku)\n\n(defn haiku
    \"It will read the form
    but will the form metadata be
    or never become?\"
    [first-five middle-seven last-five]
    (- (apply +
              ^{:last last-five} [1 2 3])
       first-five middle-seven))")

(defn test-reader
  "Return a fresh byte array input stream reading off test-bytes"
  []
  (StringReader. test-contents))

(defn meta-walk
  "Accepts an arbitrarily nested data structure form, returning a
  corresponding nested map of metadata for form and its children."
  [form]
  (if (coll? form)
    {:meta (meta form)
     :children (map meta-walk form)}
    (if (instance? clojure.lang.IMeta form)
      {:meta (meta form)
       :atom form}
      form)))

(def expected-haiku-ns-meta-walk
  {:meta {:line 1 :column 1 :end-line 1 :end-column 32 :source "(ns clojure.tools.reader.haiku)"}
   :children [{:meta {:line 1 :column 2 :end-line 1 :end-column 4 :source "ns"}
               :atom 'ns}
              {:meta {:line 1 :column 5 :end-line 1 :end-column 31 :source "clojure.tools.reader.haiku"}
               :atom 'clojure.tools.reader.haiku}]})

(deftest read-metadata
  (let [reader (-> (test-reader)
                   (LineNumberingPushbackReader.)
                   (reader-types/indexing-push-back-reader 1 "haiku.clj"))
        first-form (read reader)]
    (is (= {:line 1 :column 1 :end-line 1 :end-column 32 :source "(ns clojure.tools.reader.haiku)"} (meta first-form)))
    (is (= expected-haiku-ns-meta-walk (meta-walk first-form)))))

;; Simple benchmark of the reader reading the haiku buffer.
;; On my (aredington) laptop, this takes .163 - .168 msec to complete per run with the baseline reader.
;; With modifications to include end-line and end-column, it increases to .168 - .175 msec/run

(println (double (/ (apply + (for [i (range 1000)]
                        (let [reader (-> (test-reader)
                                         (LineNumberingPushbackReader.)
                                         (reader-types/indexing-push-back-reader 1 "haiku.clj"))
                              start (System/currentTimeMillis)]
                          (doall (take-while (comp not nil?)
                                             (repeatedly #(read reader false nil))))
                          (- (System/currentTimeMillis) start))))
             1000)))
