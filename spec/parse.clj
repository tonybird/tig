(ns parse
  (:require [parse :as sut]
            [speclj.core :refer :all]))

(describe "parse-r-flag"
          (it "removes the flag and argument"
              (should= {:root "spec" :command '("init")} (sut/parse-r-flag '("-r" "spec" "init")))))

(describe "parse-d-flag"
          (it "removes the flag and argument"
              (should= {:db ".spec" :command '("init")} (sut/parse-d-flag '("-d" ".spec" "init")))))

(describe "parse-flags"
          (it "parses when none are present"
              (should= '{:root "." :db ".idiot" :command ("init")} (sut/parse-flags '("init"))))
          (it "parses r when present"
              (should= '{:root "spec" :db ".idiot" :command ("init")} (sut/parse-flags '("-r" "spec" "init"))))
          (it "parses d when present"
              (should= '{:root "." :db ".idiot-test" :command ("init")} (sut/parse-flags '("-d" ".idiot-test" "init"))))
          (it "parses r & d present"
              (should= '{:root "spec" :db "idiot-test" :command ("init")} (sut/parse-flags '("-r" "spec" "-d" "idiot-test" "init")))))