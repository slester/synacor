(ns machine.core
  (:require [clojure.java.io :as io]
            [gloss.core :as g]
            [gloss.io :as gio]
            )
  (:import [org.apache.commons.io IOUtils])

  (:gen-class))

(def registers (vec (repeat 8 0)))
(def stack '())

(g/defcodec i16 :uint16-le)

(defn read-file [file-path]
  (IOUtils/toByteArray (io/input-stream file-path)))

(def first-register 32768)
(defn rkey [k] (- (int k) first-register))
(defn rval [rs k] (rs (rkey k)))
(defn resolve-val [rs bs v] (println "resolving" v)
  (if (> first-register v)
    (if (> first-register (bs v)) (bs v) (resolve-val rs bs (rval rs (bs v))))
    (resolve-val rs bs (rval rs v))))
(defn val-of [rs v] (if (> first-register v) v (rval rs v)))
;; (defn addr-val [rs bs v] (if (> first-register v) (bs v) (bs (rval rs v))))

(defn execute [pos mem rs s]
  "Executes the command at the given position."
  ;; (println " pos" pos "value" (mem pos) "registers:" rs "stack:" s)
  ;; (println "\n" rs s)
  (case (mem pos)
    ;; halt: 0 - stop execution and terminate the program
    0 [-1 mem rs s]
    ;; set: 1 a b - set register <a> to the value of <b>
    1 (let [a (mem (inc pos))
            b (mem (+ 2 pos))]
        ;; (println "SET: register" (rkey a) "to" (val-of rs b))
        [(+ 3 pos) mem (assoc rs (rkey a) (val-of rs b)) s])
    ;; push: 2 a - push <a> onto the stack
    2 (let [a (mem (inc pos))
            v (val-of rs a)]
        [(+ 2 pos) mem rs (conj s v)])
    ;; pop: 3 a - remove the top element from the stack and write it into <a>; empty stack = error
    3 (let [a (mem (inc pos))
            r (rkey a)
            v (peek s)]
        [(+ 2 pos) mem (assoc rs r v) (pop s)])
    ;; eq: 4 a b c - set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
    4 (let [a (mem (inc pos))
            b (mem (+ 2 pos))
            c (mem (+ 3 pos))]
        (println "EQ?" (val-of rs b) (val-of rs c) "to store in" (rkey a))
        [(+ 4 pos) mem (assoc rs (rkey (mem (inc pos)))
                              (if (= (val-of rs b) (val-of rs c)) 1 0)) s])
    ;; gt: 5 a b c - set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
    5 (let [a (mem (inc pos))
            b (mem (+ 2 pos))
            c (mem (+ 3 pos))]
        ;; (println "GT?" (val-of rs b) (val-of rs c) "to store in" (rkey a))
        [(+ 4 pos) mem (assoc rs (rkey a)
                              (if (> (val-of rs b) (val-of rs c)) 1 0)) s])
    ;; jmp: 6 a - jump to <a>
    6 (let [a (mem (inc pos))]
        ;; (println "JUMPING TO" (int a))
        [(int a) mem rs s])
    ;; jt: 7 a b -  if <a> is nonzero, jump to <b>
    7 (let [a (mem (inc pos))
            b (mem (+ 2 pos))]
        ;; (println (mem (inc pos)))
        ;; (println "JUMPING TO" (val-of rs b) "IF" (val-of rs a) "=/= 0")
        [(if (not= 0 (val-of rs a)) (int (val-of rs b)) (+ 3 pos)) mem rs s])
    ;; jf: 8 a b - if <a> is zero, jump to <b>
    8 (let [a (mem (inc pos))
            b (mem (+ 2 pos))]
        ;; (println (mem (inc pos)))
        ;; (println "JUMPING TO" (val-of rs b) "IF" (val-of rs a) "= 0")
        [(if (= 0 (val-of rs a)) (int (val-of rs b)) (+ 3 pos)) mem rs s])
    ;; add: 9 a b c - assign into <a> the sum of <b> and <c> (modulo 32768)
    9 (let [a (mem (inc pos))
            b (mem (+ 2 pos))
            c (mem (+ 3 pos))]
        ;; (println "ADDING" (val-of rs b) (val-of rs c) "to store in" (rkey a))
        [(+ 4 pos) mem (assoc rs (rkey a)
                              (mod (+ (val-of rs b) (val-of rs c)) 32768)) s])
    ;; mult: 10 a b c - store into <a> the product of <b> and <c> (modulo 32768)
    10 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             c (mem (+ 3 pos))]
         ;; (println "MULT" (val-of rs b) (val-of rs c) "to store in" (rkey a))
         [(+ 4 pos) mem (assoc rs (rkey a)
                               (mod (* (val-of rs b) (val-of rs c)) 32768)) s])
    ;; mod: 11 a b c - store into <a> the remainder of <b> divided by <c>
    11 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             c (mem (+ 3 pos))]
         ;; (println "MOD" (val-of rs b) (val-of rs c) "to store in" (rkey a))
         [(+ 4 pos) mem (assoc rs (rkey a)
                               (rem (val-of rs b) (val-of rs c))) s])
    ;; and: 12 a b c - stores into <a> the bitwise and of <b> and <c>
    12 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             c (mem (+ 3 pos))]
         ;; (println "AND" (val-of rs b) (val-of rs c) "to store in" (rkey a))
         [(+ 4 pos) mem (assoc rs (rkey a)
                               (bit-and (val-of rs b) (val-of rs c))) s])
    ;; or: 13 a b c - stores into <a> the bitwise or of <b> and <c>
    13 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             c (mem (+ 3 pos))]
         ;; (println "OR" (val-of rs b) (val-of rs c) "to store in" (rkey a))
         [(+ 4 pos) mem (assoc rs (rkey a)
                               (bit-or (val-of rs b) (val-of rs c))) s])
    ;; not: 14 a b - stores 15-bit bitwise inverse of <b> in <a>
    14 (let [a (mem (inc pos))
             b (mem (+ 2 pos))]
         ;; (println "NOT" (val-of rs b))
         [(+ 3 pos) mem (assoc rs (rkey a)
                               (bit-and 2r0111111111111111 (bit-not (val-of rs b)))) s])
    ;; rmem: 15 a b - read memory at address <b> and write it to <a>
    15 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             b-val (resolve-val rs mem b)]
         (println "RMEM at address" b "and write it to" a)
         (println "RMEM:" b-val "->" (rkey a))
         [(+ 3 pos) mem (assoc rs (rkey a) b-val) s])
    ;; wmem: 16 a b - write the value from <b> into memory at address <a>
    16 (let [a (mem (inc pos))
             b (mem (+ 2 pos))
             a-val (rval rs a)]
         (println "WMEM value from" b "into memory at address" a-val)
         (println b "->" a-val)
         [(+ 3 pos)
          (if (>= a-val first-register) mem (assoc mem (int a-val) b))
          (if (>= a-val first-register) (assoc rs (rkey a-val) b) rs) s])
    ;; call: 17 a - write the address of the next instruction to the stack and jump to <a>
    17 (let [a (mem (inc pos))]
         ;; (println "CALL adding to the stack:" (+ 2 pos) "then jumping to" (val-of rs a))
         [(int (val-of rs a)) mem rs (conj s (+ 2 pos))])
    ;; ret: 18 - remove the top element from the stack and jump to it; empty stack = halt
    18 (let [n (peek s)]
         (if (nil? n)
           [-1 rs s]
           [(int n) mem rs (pop s)]))
    ;; out: 19 a - write the character represented by ascii code <a> to the terminal
    19 (let [c (char (val-of rs (mem (inc pos))))]
         (print c)
         [(+ 2 pos) mem rs s])
    ;; in: 20 a - read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
    20 [(+ 2 pos) mem (assoc rs (rkey (mem (inc pos))) (last (read-line))) s]
    ;; noop: 21 - no operation
    21 [(inc pos) mem rs s]
    (do
      (println "unknown command")
      [-1 rs s])))

(defn main
  "Run the provided binary."
  [bin]
  (let [bs (gio/decode-all i16 (read-file bin))]
    ;; (println bs)
    (loop [pos 0
           mem bs
           rs registers
           s stack]
      (let [[new-pos new-mem new-rs new-s] (execute pos mem rs s)]
        (println " #" new-pos new-rs new-s)
        (when (> new-pos -1) (recur new-pos new-mem new-rs new-s))))))
