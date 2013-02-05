(ns clojure.tools.reader.edn
  (:refer-clojure :exclude [read read-line read-string char])
  (:require [clojure.tools.reader
             [utils :refer :all]
             [reader-types :refer :all]
             [commons :refer :all]])
  (:import (clojure.lang BigInt Numbers PersistentHashMap PersistentHashSet IMeta
                         RT IReference Symbol Reflector Var IObj
                         PersistentVector IRecord Namespace LineNumberingPushbackReader)
           java.io.InputStream
           (java.util ArrayList regex.Pattern regex.Matcher)
           java.lang.reflect.Constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare read macros dispatch-macros)

(defn macro-terminating? [ch]
  (and (not (identical? \# ch))
       (not (identical? \' ch))
       (not (identical? \: ch))
       (macros ch)))

(defn ^String read-token
  [rdr initch]
  (if-not initch
    (reader-error rdr "EOF while reading")
    (loop [sb (doto (StringBuilder.) (.append initch))
           ch (peek-char rdr)]
      (if (or (whitespace? ch)
              (macro-terminating? ch)
              (nil? ch))
        (str sb)
        (recur (doto sb (.append (read-char rdr))) (peek-char rdr))))))

(declare read-tagged)

(defn read-dispatch
  [rdr _]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch)
      (if-let [obj (read-tagged (doto rdr (unread ch)) ch)]
        obj
        (reader-error rdr "No dispatch macro for " ch)))
    (reader-error rdr "EOF while reading character")))

(defn read-unmatched-delimiter
  [rdr ch]
  (reader-error rdr "Unmatched delimiter " ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-unicode-char
  ([^String token offset length base]
     (let [l (+ offset length)]
       (when-not (== (count token) l)
         (throw (IllegalArgumentException. (str "Invalid unicode character: \\" token))))
       (loop [i offset uc 0]
         (if (== i l)
           (char uc)
           (let [d (Character/digit ^char (nth token i) ^int base)]
             (if (== d -1)
               (throw (IllegalArgumentException. (str "Invalid digit: " (nth token i))))
               (recur (inc i) (long (+ d (* uc base))))))))))

  ([rdr initch base length exact?]
     (loop [i 1 uc (Character/digit ^char initch ^int base)]
       (if (== uc -1)
         (throw (IllegalArgumentException. (str "Invalid digit: " initch)))
         (if-not (== i length)
           (let [ch (peek-char rdr)]
             (if (or (whitespace? ch)
                     (macros ch)
                     (nil? ch))
               (if exact?
                 (throw (IllegalArgumentException.
                         (str "Invalid character length: " i ", should be: " length)))
                 (char uc))
               (let [d (Character/digit ^char ch ^int base)]
                 (read-char rdr)
                 (if (== d -1)
                   (throw (IllegalArgumentException. (str "Invalid digit: " ch)))
                   (recur (inc i) (long (+ d (* uc base))))))))
           (char uc))))))

(let [upper-limit (int \uD7ff)
      lower-limit (int \uE000)]
  (defn read-char*
    [rdr backslash]
    (let [ch (read-char rdr)]
      (if-not (nil? ch)
        (let [token (read-token rdr ch)
              token-len (count token)]
          (cond

           (== 1 token-len)  (Character/valueOf (nth token 0))

           (= token "newline") \newline
           (= token "space") \space
           (= token "tab") \tab
           (= token "backspace") \backspace
           (= token "formfeed") \formfeed
           (= token "return") \return

           (.startsWith token "u")
           (let [c (read-unicode-char token 1 4 16)
                 ic (int c)]
             (if (and (> ic upper-limit)
                      (< ic lower-limit))
               (reader-error rdr "Invalid character constant: \\u" (Integer/toString ic 16))
               c))

           (.startsWith token "x")
           (read-unicode-char token 1 2 16)

           (.startsWith token "o")
           (let [len (dec token-len)]
             (if (> len 3)
               (reader-error rdr "Invalid octal escape sequence length: " len)
               (let [uc (read-unicode-char token 1 len 8)]
                 (if (> (int uc) 0377)
                   (reader-error rdr "Octal escape sequence must be in range [0, 377]")
                   uc))))

           :else (reader-error rdr "Unsupported character: \\" token)))
        (reader-error rdr "EOF while reading character")))))

(defn ^PersistentVector read-delimited
  [delim rdr recursive?]
  (let [first-line  (when (indexing-reader? rdr)
                      (get-line-number rdr))
        delim ^char delim]
    (loop [a (transient [])]
      (let [ch (read-past whitespace? rdr)]
        (when-not ch
          (reader-error rdr "EOF while reading"
                        (if first-line
                          (str ", starting at line" first-line))))
        (if (identical? delim ^char ch)
          (persistent! a)
          (if-let [macrofn (macros ch)]
            (let [mret (macrofn rdr ch)]
              (recur (if-not (identical? mret rdr) (conj! a mret) a)))
            (let [o (read (doto rdr (unread ch)) true nil recursive?)]
              (recur (if-not (identical? o rdr) (conj! a o) a)))))))))

(defn read-list
  [rdr _]
  (let [[line column] (when (indexing-reader? rdr)
                        [(get-line-number rdr) (dec (get-column-number rdr))])
        the-list (read-delimited \) rdr true)]
    (if (empty? the-list)
      '()
      (with-meta (clojure.lang.PersistentList/create the-list)
        (when line
          {:line line :column column})))))

(defn read-vector
  [rdr _]
  (let [[line column] (when (indexing-reader? rdr)
                        [(get-line-number rdr) (dec (get-column-number rdr))])
        the-vector (read-delimited \] rdr true)]
    (with-meta the-vector
      (when line
        {:line line :column column}))))

(defn read-map
  [rdr _]
  (let [[line column] (when (indexing-reader? rdr)
                        [(get-line-number rdr) (dec (get-column-number rdr))])
        l (to-array (read-delimited \} rdr true))]
    (when (== 1 (bit-and (alength l) 1))
      (reader-error rdr "Map literal must contain an even number of forms"))
    (with-meta (RT/map l)
      (when line
        {:line line :column column}))))

(defn read-number
  [reader initch]
  (loop [sb (doto (StringBuilder.) (.append initch))
         ch (read-char reader)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)]
        (unread reader ch)
        (or (match-number s)
            (reader-error reader "Invalid number format [" s "]")))
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (== -1 (Character/digit ^char ch 16))
             (reader-error rdr "Invalid unicode escape: \\u" ch)
             (read-unicode-char rdr ch 16 4 true)))
      \x (let [ch (read-char rdr)]
           (if (== -1 (Character/digit ^char ch 16))
             (reader-error rdr "Invalid unicode escape: \\x" ch)
             (read-unicode-char rdr ch 16 2 true)))
      (if (numeric? ch)
        (let [ch (read-unicode-char rdr ch 8 3 false)]
          (if (> (int ch) 0337)
            (reader-error rdr "Octal escape sequence must be in range [0, 377]")
            ch))
        (reader-error rdr "Unsupported escape character: \\" ch)))))

(defn read-string*
  [reader _]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (case ch
      nil (reader-error reader "EOF while reading string")
      \\ (recur (doto sb (.append (escape-char sb reader)))
                (read-char reader))
      \" (str sb)
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn read-symbol
  [rdr initch]
  (when-let [token (read-token rdr initch)]
    (let [[line column] (when (indexing-reader? rdr)
                          [(get-line-number rdr) (dec (get-column-number rdr))])]
      (case token

        ;; special symbols
        "nil" nil
        "true" true
        "false" false
        "/" '/
        "NaN" Double/NaN
        "-Infinity" Double/NEGATIVE_INFINITY
        ("Infinity" "+Infinity") Double/POSITIVE_INFINITY

        (or (when-let [p (parse-symbol token)]
              (with-meta (symbol (p 0) (p 1))
                (when line
                  {:line line :column column})))
            (reader-error rdr "Invalid token: " token))))))

(defn read-keyword
  [reader initch]
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader ch)
            s (parse-symbol token)]
        (if (and s (== -1 (.indexOf token "::")))
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (reader-error reader "Invalid token: :" token) ;; no ::keyword in edn
              (keyword ns name)))
          (reader-error reader "Invalid token: :" token)))
      (reader-error reader "Invalid token: :"))))

(defn wrapping-reader
  [sym]
  (fn [rdr _]
    (list sym (read rdr true nil true))))

(defn read-meta
  [rdr _]
  (let [[line column] (when (indexing-reader? rdr)
                        [(get-line-number rdr) (dec (get-column-number rdr))])
        m (desugar-meta (read rdr true nil true))]
    (when-not (map? m)
      (reader-error rdr "Metadata must be Symbol, Keyword, String or Map"))
    (let [o (read rdr true nil true)]
      (if (instance? IMeta o)
        (let [m (if (and line
                         (seq? o))
                  (assoc m :line line
                           :column column)
                  m)]
          (when (instance? IObj o)
            (with-meta o (merge (meta o) m))))
        (reader-error rdr "Metadata can only be applied to IMetas")))))

(defn read-set
  [rdr _]
  (PersistentHashSet/createWithCheck (read-delimited \} rdr true)))

(defn read-discard
  [rdr _]
  (read rdr true nil true)
  rdr)

(defn macros [ch]
  (case ch
    \" read-string*
    \: read-keyword
    \; read-comment
    \' (throwing-reader "Not an EDN form")
    \@ (throwing-reader "Not an EDN form")
    \^ read-meta
    \` (throwing-reader "Not an EDN form")
    \~ (throwing-reader "Not an EDN form")
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \% (throwing-reader "Not an EDN form")
    \# read-dispatch
    nil))

(defn dispatch-macros [ch]
  (case ch
    \^ read-meta                ;deprecated
    \' (throwing-reader "Not an EDN form")
    \( (throwing-reader "Not an EDN form")
    \= (throwing-reader "Not an EDN form")
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \" read-regex
    \! read-comment
    \_ read-discard
    nil))

(defn read-tagged [rdr initch]
  (let [tag (read rdr true nil false)
        object (read rdr true nil true)]
    (if-not (symbol? tag)
      (reader-error rdr "Reader tag must be a symbol"))
    (if-let [f (or (*data-readers* tag)
                   (default-data-readers tag)
                   @default-data-reader-fn)]
      (reader-error rdr "No reader function for tag " (name tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
Returns the object read. If EOF, throws if eof-error? is true. Otherwise returns sentinel."
  ([] (read *in*))
  ([reader] (read reader true nil))
  ([reader eof-error? sentinel] (read reader eof-error? sentinel false))
  ([reader eof-error? sentinel recursive?]
     (try
       (let [ch (read-char reader)]
         (cond
          (whitespace? ch) (read reader eof-error? sentinel recursive?)
          (nil? ch) (if eof-error? (reader-error reader "EOF") sentinel)
          (number-literal? reader ch) (read-number reader ch)
          (comment-prefix? ch) (read (read-comment reader ch) eof-error? sentinel recursive?)
          :else (let [f (macros ch)]
                  (if f
                    (let [res (f reader ch)]
                      (if (identical? res reader)
                        (read reader eof-error? sentinel recursive?)
                        res))
                    (read-symbol reader ch)))))
       (catch Exception e
         (if (instance? clojure.lang.ExceptionInfo e)
           (throw e)
           (throw (ex-info (.getMessage e)
                           (merge {:type :reader-exception}
                                  (if (indexing-reader? reader)
                                    {:line (get-line-number reader)
                                     :column (get-column-number reader)}))
                           e)))))))

(defn read-string
  "Reads one object from the string s"
  [s]
  (read (string-push-back-reader s) true nil false))
