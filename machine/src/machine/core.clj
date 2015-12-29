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

(defn rkey [k] (- (int k) 32768))
(defn rval [rs k] (rs (rkey k)))
(defn val-of [rs v] (if (> 32768 v) v (rval rs v)))
(defn addr-val [rs bs v] (if (> 32768 v) (bs v) (bs (rval rs v))))

(defn execute [bs pos rs s]
  "Executes the command at the given position."
  ;; (println " pos" pos "value" (bs pos) "registers:" rs "stack:" s)
  ;; (println "\n" rs s)
 (case (bs pos)
   ;; halt: 0 - stop execution and terminate the program
   0 [-1 rs s]
   ;; set: 1 a b - set register <a> to the value of <b>
   1 (do
       (println "SET: register" (rkey (bs (inc pos))) "to" (val-of rs (bs (+ 2 pos))))
        [(+ 3 pos) (assoc rs (rkey (bs (inc pos))) (val-of rs (bs (+ 2 pos)))) s])
   ;; push: 2 a - push <a> onto the stack
   2 (let [v (val-of rs (bs (inc pos)))]
       [(+ 2 pos) rs (conj s v)])
   ;; pop: 3 a - remove the top element from the stack and write it into <a>; empty stack = error
   3 (let [r (rkey (bs (inc pos)))
           v (peek s)]
       [(+ 2 pos) (assoc rs r v) (pop s)])
   ;; eq: 4 a b c - set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
   4 (do
      (println "EQ?" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
       [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                          (if (= (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos)))) 1 0)) s])
   ;; gt: 5 a b c - set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
   5 (do
       (println "GT?" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
       [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                          (if (> (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos)))) 1 0)) s])
   ;; jmp: 6 a - jump to <a>
   6 (do
       (println "JUMPING TO" (int (bs (inc pos))))
       [(int (bs (inc pos))) rs s])
   ;; jt: 7 a b -  if <a> is nonzero, jump to <b>
   7 (do
       ;; (println (bs (inc pos)))
       (println "JUMPING TO" (val-of rs (bs (+ 2 pos))) "IF" (val-of rs (bs (inc pos))) "=/= 0")
       [(if (not= 0 (val-of rs (bs (inc pos)))) (int (val-of rs (bs (+ 2 pos)))) (+ 3 pos)) rs s])
   ;; jf: 8 a b - if <a> is zero, jump to <b>
   8 (do
       ;; (println (bs (inc pos)))
       (println "JUMPING TO" (val-of rs (bs (+ 2 pos))) "IF" (val-of rs (bs (inc pos))) "= 0")
       [(if (= 0 (val-of rs (bs (inc pos)))) (int (val-of rs (bs (+ 2 pos)))) (+ 3 pos)) rs s])
   ;; add: 9 a b c - assign into <a> the sum of <b> and <c> (modulo 32768)
   9 (do
       (println "ADDING" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
       [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                          (mod (+ (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos)))) 32768)) s])
   ;; mult: 10 a b c - store into <a> the product of <b> and <c> (modulo 32768)
   10 (do
        (println "MULT" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
        [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                           (mod (* (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos)))) 32768)) s])
   ;; mod: 11 a b c - store into <a> the remainder of <b> divided by <c>
   11 (do
        (println "MOD" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
        [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                           (rem (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))))) s])
   ;; and: 12 a b c - stores into <a> the bitwise and of <b> and <c>
   12 (do
        (println "AND" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
        [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                           (bit-and (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))))) s])
   ;; or: 13 a b c - stores into <a> the bitwise or of <b> and <c>
   13 (do
        (println "OR" (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))) "to store in" (rkey (bs (inc pos))))
        [(+ 4 pos) (assoc rs (rkey (bs (inc pos)))
                           (bit-or (val-of rs (bs (+ 2 pos))) (val-of rs (bs (+ 3 pos))))) s])
   ;; not: 14 a b - stores 15-bit bitwise inverse of <b> in <a>
   14 (do
        (println "NOT" (val-of rs (bs (+ 2 pos))))
        [(+ 3 pos) (assoc rs (rkey (bs (inc pos)))
                        (bit-and 2r0111111111111111 (bit-not (val-of rs (bs (+ 2 pos)))))) s])
   ;; rmem: 15 a b - read memory at address <b> and write it to <a>
   15 (do
        (println "RMEM at address" (bs (+ 2 pos)) "and write it to" (bs (inc pos)))
        (println "RMEM:" (addr-val rs bs (bs (+ 2 pos))) "->" (rkey (bs (inc pos))))
        [(+ 3 pos) (assoc rs (rkey (bs (inc pos))) (addr-val rs bs (bs (+ 2 pos)))) s])
   ;; wmem: 16 a b - write the value from <b> into memory at address <a>
   16 (do
        (println "WMEM value from" (bs (+ 2 pos)) "into memory at address" (bs (inc pos)))
        ;; (println "WMEM: bs value" (bs (bs (+ 2 pos))))
        ;; (println "WMEM:" (addr-val rs bs (bs (+ 2 pos))) "->" (rkey (bs (inc pos))))
        ;; (println "WMEM value from" (addr-val rs bs (bs (+ 2 pos))) "into memory at address" (rkey (bs (inc pos))))
        [(+ 3 pos) (assoc rs (rkey (bs (inc pos))) (bs (+ 2 pos))) s])
   ;; call: 17 a - write the address of the next instruction to the stack and jump to <a>
   17 (do
        (println "CALL adding to the stack:" (+ 2 pos) "then jumping to" (val-of rs (bs (inc pos))))
        [(int (val-of rs (bs (inc pos)))) rs (conj s (+ 2 pos))])
   ;; ret: 18 - remove the top element from the stack and jump to it; empty stack = halt
   18 (let [n (peek s)]
        (if (nil? n)
          [-1 rs s]
          [(int n) rs (pop s)]))
   ;; out: 19 a - write the character represented by ascii code <a> to the terminal
   19 (let [c (char (val-of rs (bs (inc pos))))]
        (print c)
        [(+ 2 pos) rs s])
   ;; in: 20 a - read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
   20 [(+ 2 pos) (assoc rs (rkey (bs (inc pos))) (read-line)) s]
   ;; noop: 21 - no operation
   21 [(inc pos) rs s]
   (do
    (println "unknown command")
     [-1 rs s])))

(defn main
  "Run the provided binary."
  [bin]
  (let [bs (gio/decode-all i16 (read-file bin))]
    ;; (println bs)
   (loop [pos 0
          rs registers
          s stack]
     (let [[new-pos new-rs new-s] (execute bs pos rs s)]
       (println " #" new-pos new-rs new-s)
       (when (> new-pos -1) (recur new-pos new-rs new-s))))))
