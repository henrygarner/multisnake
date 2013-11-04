(ns multiplay.utils)

(defn log
  [obj]
  (comment (.log js/console (pr-str obj))))

(def host
  (aget js/window "location" "host"))
