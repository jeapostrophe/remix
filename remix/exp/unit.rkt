;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang remix
;; xxx make real
#;
(require* remix/unit0)
#;#;
(import foo) ; =>
(import [(^exactly foo) foo])
#;#;
(require remix/set0)
(import [(^prefix bid: ^set) remix/stx/bound-id]
        [(^prefix bit: ^set) (remix/set/bit 20)])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
