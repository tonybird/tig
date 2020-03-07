(ns parse
  (:require [parse :as sut]
            [speclj.core :refer :all]))

(describe "parse-r-flag"
          (it "removes the flag and argument"
              (should= {:r "spec" :command '("init")} (sut/parse-r-flag '("-r" "spec" "init")))))

(describe "parse-d-flag"
          (it "removes the flag and argument"
              (should= {:d ".spec" :command '("init")} (sut/parse-d-flag '("-d" ".spec" "init")))))

(describe "parse-flags"
          (it "parses when none are present"
              (should= '{:r "." :d ".idiot" :command ("init")} (sut/parse-flags '("init"))))
          (it "parses r when present"
              (should= '{:r "spec" :d ".idiot" :command ("init")} (sut/parse-flags '("-r" "spec" "init"))))
          (it "parses d when present"
              (should= '{:r "." :d ".idiot-test" :command ("init")} (sut/parse-flags '("-d" ".idiot-test" "init"))))
          (it "parses r & d present"
              (should= '{:r "spec" :d "idiot-test" :command ("init")} (sut/parse-flags '("-r" "spec" "-d" "idiot-test" "init")))))