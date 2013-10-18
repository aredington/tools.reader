;:   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.tools.reader.impl.utils
  (:require clojure.tools.reader.impl.ExceptionInfo) ;; force loading
  (:refer-clojure :exclude [char]))

(defn char [x]
  (when x
    (clojure.core/char x)))

;; getColumnNumber and *default-data-reader-fn* are available only since clojure-1.5.0-beta1
(def >=clojure-1-5-alpha*?
  (let [{:keys [minor qualifier]} *clojure-version*]
    (or (and (= minor 5)
             (not= "alpha"
                   (when qualifier
                     (subs qualifier 0 (dec (count qualifier))))))
        (> minor 5))))

(defmacro compile-if [cond then else]
  (if (eval cond)
    then
    else))

(compile-if (= 3 (:minor *clojure-version*))
  (do
    (defn ex-info
      ([msg map]
         (clojure.tools.reader.impl.ExceptionInfo. msg map))
      ([msg map cause]
         (clojure.tools.reader.impl.ExceptionInfo. msg map cause)))
    (defn ex-data
      [^clojure.tools.reader.impl.ExceptionInfo ex]
      (.getData ex))
    (defn ex-info? [ex]
      (instance? clojure.tools.reader.impl.ExceptionInfo ex)))

    (defn ex-info? [ex]
      (instance? clojure.lang.ExceptionInfo ex)))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Character/isWhitespace ^Character ch)
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (Character/isDigit ch)))

(defn comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (identical? \;  ch))

(defn newline? [c]
  "Checks whether the character is a newline"
  (or (identical? \newline c)
      (nil? c)))

(defn desugar-meta
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    :else        f))

(defn merge-meta
  "Returns an object of the same type and value as `obj`, with its
  metadata merged over `m`."
  [obj m]
  (let [orig-meta (meta obj)]
    (with-meta obj (merge m orig-meta))))

(def ^:private ^:dynamic *source-log-frames*
  {})

(defn- peek-source-log
  "Returns a string containing the contents of the top most source
  logging frame."
  []
  (.substring ^StringBuilder (:buffer *source-log-frames*) (:offset *source-log-frames*)))

(defn log-source-char
  "Logs `char` to all currently active source logging frames."
  [char]
  (when-let [^StringBuilder buffer (:buffer *source-log-frames*)]
    (.append buffer char)))

(defn drop-last-logged-char
  "Removes the last logged character from all currently active source
  logging frames. Called when pushing a character back."
  []
  (when-let [^StringBuilder buffer (:buffer *source-log-frames*)]
    (.deleteCharAt buffer (dec (.length buffer)))))

(defn log-source-call
  [f]
  (let [new-frame (if (thread-bound? #'*source-log-frames*)
                    (assoc *source-log-frames* :offset (.length ^StringBuilder (:buffer *source-log-frames*)))
                    {:buffer (StringBuilder.)
                     :offset 0})]
    (binding [*source-log-frames* new-frame]
      (let [ret (f)]
        (if (instance? clojure.lang.IMeta ret)
          (merge-meta ret {:source (peek-source-log)})
          ret)))))

(defmacro log-source
  "Executes `body` after pushing a new frame to the source logging
  stack. If `body` returns a value that extends IMeta, retrieves the
  source logged during body's execution, and attaches it to the
  value's meta under the :source key. Tears down the source logging
  stack frame under all circumstances."
  [& body]
  `(log-source-call (fn [] ~@body)))
