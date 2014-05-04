(ns build-hook
  (:require [clojure.string :as string]
            [mbwatch.command :refer [OPCODE-HELP]]))

(defn -main
  "A build hook, not intended for runtime execution."
  []
  (let [bin "bin/mbwatch-client"
        help (str "-*- COMMAND HELP -*-\n"
                  "read -r -d '' HELP <<'ENDOFHELP'\n"
                  OPCODE-HELP "\n"
                  "ENDOFHELP")
        buf (-> (slurp bin)
                (string/replace #"(?s)\Q-*- COMMAND HELP -*-\E.*\nENDOFHELP" help)
                (string/replace #"(?m)[ \t]+$" ""))]
    (spit bin buf)))
