(ns fruit-economy.language
  (:require [clojure.string :as str])
  (:import [squidpony FakeLanguageGen NaturalLanguageCipher]))


(defn make-lang []
  (let [language (FakeLanguageGen/randomLanguage
                   (bit-xor
                     (long (* (- (rand) 0.5) 0x10000000000000))
                     (long (* (- (rand) 0.5) 2 0x8000000000000000))))]
    language))

(defn make-word
  ([lang] (make-word lang true))
  ([lang capitalise?]
   (.word lang capitalise?)))


(comment
  (let [language (FakeLanguageGen/randomLanguage
                   (bit-xor
                     (long (* (- (rand) 0.5) 0x10000000000000))
                     (long (* (- (rand) 0.5) 2 0x8000000000000000))))]
    #_(.word language true)
    language)

  (let [lang FakeLanguageGen/DEEP_SPEECH]
    (.word lang true))

  (let [translator (NaturalLanguageCipher. FakeLanguageGen/DEEP_SPEECH)]
    (.cipher translator "Civ %+7" #_"This is a Fruit Economy")
    #_(.cipherMarkup translator "This [?]is[?] a Fruit Economy")
    #_(clojure.reflect/reflect translator))

  (clojure.reflect/reflect FakeLanguageGen/DEEP_SPEECH)
  (clojure.reflect/reflect NaturalLanguageCipher))
(comment
  (def dict
    (let [dict-path "/usr/share/dict/words"]
      (-> (slurp dict-path)
        (java.io.StringReader.)
        (java.io.BufferedReader.)
        (line-seq))))

  (def gdict
    (let [dict-path "/Users/folcon/Downloads/google-10000-english.txt"]
      (-> (slurp dict-path)
        (java.io.StringReader.)
        (java.io.BufferedReader.)
        (line-seq))))

  (def bdict
    (let [dict-path "/Users/folcon/Downloads/words.txt"]
      (-> (slurp dict-path)
        (java.io.StringReader.)
        (java.io.BufferedReader.)
        (line-seq))))

  (def oeddict
    (let [dict-path "/Users/folcon/Downloads/frequency-alpha-oed.txt"]
      (-> (slurp dict-path)
        (java.io.StringReader.)
        (java.io.BufferedReader.)
        (line-seq))))

  (def gbookddict
    (let [dict-path "/Users/folcon/Downloads/frequency-all-words.txt"]
      (-> (slurp dict-path)
        (java.io.StringReader.)
        (java.io.BufferedReader.)
        (line-seq)))))

(defn include [words letter]
  (into [] (keep #(when (str/includes? % letter) %)) words))

(defn exclude [letters ex]
  (into [] (remove #(some (set [%]) ex)) letters))

(defn to-class [letters]
  (str "[" (str/join letters) "]"))

(def str->v (comp (distinct) (map str)))

(defn ->v [letters]
  (if (coll? letters)
    (into [] (comp cat str->v) letters)
    (into [] str->v letters)))

(comment
  (let [words #_(take 2000)
        gbookddict
        letters (->v "abcdefghijklkmopqrstuvwxyz")
        pattern (str
                  (to-class (exclude letters (->v ["earth" "bws" "fdy" "ovi" "ovi" "oviqu" "ebfoq"])))
                  (to-class (exclude letters (->v ["earth" "bws" "fdy" "lo" "ovi" "oviqu" "alovu"])))
                  "o"
                  "l"
                  "l")]
    (->>
      words
      (into []
        (comp
          (map
            (fn [word]
              (when-let [w (re-find (re-pattern (str "(?i)" pattern)) word)]
                 (re-matches #".*l.*" w))))
          (remove nil?)
          (distinct))))))